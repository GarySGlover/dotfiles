# [[file:../modules.org::*Kubernetes][Kubernetes:1]]
{
  flake.aspects = {
    options = {
      homeManager =
        { lib, ... }:
        {
          options.kubernetes.includeKubelogin = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Enable kubelogin for kubectl";
          };
        };
    };
    kubernetes = {
      homeManager =
        {
          pkgs,
          config,
          lib,
          inputs,
          ...
        }:
        {
          config = lib.mkMerge [
            {
              programs.k9s.enable = true;
              home.packages = with pkgs; [
                kubectl
                kubernetes-helm
                helm-dashboard
                (inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.sofka)
              ];
            }
            (lib.mkIf config.kubernetes.includeKubelogin {
              home.packages = with pkgs; [ kubelogin ];
            })
          ];
        };
    };
  };
  perSystem =
    { pkgs, ... }:
    {

      packages.sofka = pkgs.rustPlatform.buildRustPackage (finalAttrs: {
        pname = "sofka";
        version = "0.31.8";

        src = pkgs.fetchFromGitHub {
          owner = "nklmilojevic";
          repo = "sofka";
          tag = "v${finalAttrs.version}";
          hash = "sha256-KIIc6Ls70z4mh+eUuc0YrdqU4XqtoinDRuz1uiRBHpk=";
        };

        cargoHash = "sha256-tGy7TnUmB5SIYPbdoUmn45datJKlStSOVuaDKRyRdRU=";

        # aws-lc-sys (the TLS client's crypto backend) builds AWS-LC's C sources
        # with cmake. Bindings are pregenerated for all four platforms this package
        # targets, so no bindgen/libclang is needed here.
        #
        # `dontCargoUseSystemLibs` disables nixpkgs' `cargoUseSystemLibs` hook,
        # which otherwise sets AWS_LC_SYS_USE_SYSTEM=1/ZSTD_SYS_USE_PKG_CONFIG=1/…
        # and expects every vendored `-sys` crate to be backed by a system
        # library — which then fails for any library not added to buildInputs.
        nativeBuildInputs = [ pkgs.cmake ];
        dontCargoUseSystemLibs = true;

        # The test suite builds a rustls-backed kube::Client (even for the "fake"
        # test cluster), which needs native root CA certs — unavailable in Nix's
        # network-less build sandbox. Tests run properly in CI via `cargo test`;
        # this only skips re-running them inside the hermetic package build.
        doCheck = false;

        meta = {
          description = "A Kubernetes TUI, reimagined in Rust";
          homepage = "https://github.com/nklmilojevic/sofka";
          mainProgram = "sofka";
          platforms = pkgs.lib.platforms.unix;
          license = with pkgs.lib.licenses; [
            mit
            asl20
          ];
        };
      });
    };
}
# Kubernetes:1 ends here
