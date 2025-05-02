#!/usr/bin/env bash

# Get Azure DevOps organization URL
DEVOPS_CONFIG=$(az devops configure --list | grep "^organization\s*=\s*[a-zA-Z]") || {
	echo "Error: No default DevOps organization set." >&2
	exit 1
}
readonly DEVOPS_CONFIG

DEVOPS_URL=$(echo "$DEVOPS_CONFIG" | sed -n 's/^organization\s*=\s*\(.*\)/\1/p')
readonly DEVOPS_URL
DEVOPS_ORG=$(basename "$DEVOPS_URL")
readonly DEVOPS_ORG

# Create a temporary file for caching the Azure token
TOKEN_CACHE_FILE=$(mktemp)
trap 'rm -f "$TOKEN_CACHE_FILE"' EXIT

# Generate a secure encryption key for this script run
ENCRYPTION_KEY=$(openssl rand -base64 32)

# Function to encrypt data
function encrypt_data() {
	echo "$1" | openssl enc -aes-256-cbc -a -salt -pbkdf2 -pass pass:"$ENCRYPTION_KEY"
}

# Function to decrypt data
function decrypt_data() {
	echo "$1" | openssl enc -aes-256-cbc -a -d -salt -pbkdf2 -pass pass:"$ENCRYPTION_KEY"
}

# Function to get an Azure token
function get_azure_token() {
	# Check if the token cache file exists and has content
	if [[ -s $TOKEN_CACHE_FILE ]]; then
		# Read and decrypt the cached token
		encrypted_token=$(cat "$TOKEN_CACHE_FILE")
		AZURE_TOKEN=$(decrypt_data "$encrypted_token")
		expires_on=$(echo "$AZURE_TOKEN" | jq -r '.expires_on')
		current_time=$(date +%s)
		expires_on_buffered_time=$((expires_on - 10 * 60)) # 10 minutes early expiry

		# Check if the token is still valid
		if ((expires_on_buffered_time > current_time)); then
			echo "$AZURE_TOKEN" | jq -r '.accessToken'
			return
		fi
	fi

	# Fetch a new token if not set or expired
	AZURE_TOKEN=$(az account get-access-token -o json) || {
		echo "Error: Failed to get Azure token." >&2
		exit 1
	}

	# Encrypt and cache the new token in the temporary file
	encrypted_token=$(encrypt_data "$AZURE_TOKEN")
	echo "$encrypted_token" >"$TOKEN_CACHE_FILE"

	# Return the token
	echo "$AZURE_TOKEN" | jq -r '.accessToken'
}

# Function to make authenticated Azure REST API calls
function azure_get() {
	local url="$1"
	BEARER=$(get_azure_token)
	curl -s -X GET -H "Authorization: Bearer $BEARER" "$url"
}

# Get all projects in the organization
DEVOPS_PROJECTS=$(azure_get "$DEVOPS_URL/_apis/projects?api-version=7.0") || {
	echo "Error: Failed to retrieve projects." >&2
	exit 1
}
readonly DEVOPS_PROJECTS

# Loop through each project
for PROJECT in $(echo "$DEVOPS_PROJECTS" | jq -r '.value[].name'); do
	echo "Processing project: $PROJECT"

	# Get all repositories in the project
	DEVOPS_REPOS=$(azure_get "$DEVOPS_URL/$PROJECT/_apis/git/repositories?api-version=7.0") || {
		echo "Error: Failed to retrieve repositories for project $PROJECT." >&2
		continue
	}

	# Ensure project directory exists
	project_dir="$HOME/git-backups/$DEVOPS_ORG/$PROJECT"
	mkdir -p "$project_dir"

	# Output the repositories to a file
	echo "$DEVOPS_REPOS" | jq -r '.' >"$project_dir/repositories.json"
	# Output repos policy to a file
	azure_get "$DEVOPS_URL/$PROJECT/_apis/policy/configurations?api-version=7.0" | jq -r '.' >"$project_dir/policy.json"
	# Output pipelines to a file
	azure_get "$DEVOPS_URL/$PROJECT/_apis/pipelines?api-version=7.0" | jq -r '.' >"$project_dir/pipelines.json"
	# Output variable groups to a file
	azure_get "$DEVOPS_URL/$PROJECT/_apis/distributedtask/variablegroups?api-version=7.0" | jq -r '.' >"$project_dir/variablegroups.json"
	# Output service connections to a file
	azure_get "$DEVOPS_URL/$PROJECT/_apis/serviceendpoint/endpoints?api-version=7.0" | jq -r '.' >"$project_dir/serviceconnections.json"
	# Output wikis to a file
	azure_get "$DEVOPS_URL/$PROJECT/_apis/wiki/wikis?api-version=7.0" | jq -r '.' >"$project_dir/wikis.json"

	# Loop through each repository
	for REPO in $(echo "$DEVOPS_REPOS" | jq -r '.value[].name'); do
		echo "Processing repository: $REPO"

		# Define the folder structure
		path="$HOME/git-backups/$DEVOPS_ORG/$PROJECT/$REPO"
		mkdir -p "$(dirname "$path")"

		# Clone or update the repository
		if [ -d "$path" ]; then
			echo "Repository $REPO already exists. Updating..."
			pushd "$path" >/dev/null || continue
			git fetch --all --prune --tags || {
				echo "Error: Failed to fetch updates for $REPO." >&2
				popd >/dev/null || continue
			}
			popd >/dev/null || continue
		else
			echo "Cloning repository $REPO with all branches and history..."
			url=$(echo "$DEVOPS_REPOS" | jq --arg repo "$REPO" -r '.value[] | select(.name==$repo) | .remoteUrl')
			git clone --mirror "$url" "$path" || {
				echo "Error: Failed to clone $REPO." >&2
				continue
			}
		fi
	done
done
