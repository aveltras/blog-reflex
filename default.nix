{ system ? builtins.currentSystem }:

let

  githubTarball = owner: repo: rev:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

  reflexPlatformSrc = githubTarball "reflex-frp" "reflex-platform" "df43a921befd11e5635dc604e9911c2822ac42ec";
  obeliskSrc = githubTarball "obsidiansystems" "obelisk" "v0.8.0.0";
  morpheusSrc = githubTarball "morpheusgraphql" "morpheus-graphql" "7629168d4641d96b5a12dc20ab606fb54bea68ad";
  morpheusSrcLocal = ./../morpheus-graphql;
  
in

(import reflexPlatformSrc { inherit system; }).project ({ pkgs, ... }: {

  useWarp = true;
  withHoogle = false;

  packages = {
    blog-reflex = ./.;
  };

  shells = {
    ghc = ["blog-reflex"];
    ghcjs = ["blog-reflex"];
  };

  
  
  overrides = self: super: with pkgs.haskell.lib; let
    waiStaticSrc = pkgs.nix-gitignore.gitignoreSourcePure [./../wai-static-th/.gitignore] ./../wai-static-th;
    sessionSrc = pkgs.nix-gitignore.gitignoreSourcePure [./../sessionula/.gitignore] ./../sessionula;
  in {
    wai-static-th = dontCheck (super.callCabal2nix "wai-static-th" waiStaticSrc {});
                                                                                                           
    HsYAML-aeson = self.callHackageDirect { pkg = "HsYAML-aeson"; ver = "0.2.0.0"; sha256 = "0zgcp93y93h7rsg9dv202hf3l6sqr95iadd67lmfclb0npfs640m"; } {};
    HsYAML = self.callHackageDirect { pkg = "HsYAML"; ver = "0.2.1.0"; sha256 = "0r2034sw633npz7d2i4brljb5q1aham7kjz6r6vfxx8qqb23dwnc"; } {};
    stylish-haskell = self.callHackageDirect { pkg = "stylish-haskell"; ver = "0.11.0.0"; sha256 = "1a6jijj1lxmi20m9ddiwlnlf3x6qy3dw4si1pvfk9rpjv9azcydk"; } {};
    kind-apply = self.callHackageDirect { pkg = "kind-apply"; ver = "0.3.2.0"; sha256 = "1pfcq42pvpcybh48mjga7jfjj792gcfxpgzb083dnr2n8ryhlmgf"; } {};
    kind-generics = self.callHackageDirect { pkg = "kind-generics"; ver = "0.4.1.0"; sha256 = "1ryzmfzzns50ci43qhldshg6rfskq61z5807917llbp90ps5ngw0"; } {};
    kind-generics-th = self.callHackageDirect { pkg = "kind-generics-th"; ver = "0.2.2.0"; sha256 = "1vi9qv62rcm7nb5qylx8gzb1bzwna2p9mmzc35abz967vzg47h2s"; } {};

    squeal-postgresql = dontCheck (super.callHackageDirect { pkg = "squeal-postgresql"; ver = "0.6.0.2"; sha256 = "1vq7c4yn1sz34da9y9brq2g1yjsp8vf94g7iz5wzrcvgap5jl6gx"; } {});
    with-utf8 = super.callHackageDirect { pkg = "with-utf8"; ver = "1.0.1.0"; sha256 = "129bsyawcmfig1m3cch91d4nn6wlji3g5lm26jkf08yp54l76lrq"; } {};
    free-categories = super.callHackageDirect { pkg = "free-categories"; ver = "0.2.0.0"; sha256 = "1grlvy8r7nbb7g8sx5a5x6az03bzli510zjpp5dpliidvajncci9"; } {};

    sessionula = dontCheck (super.callCabal2nix "sessionula" "${sessionSrc}/sessionula" {});
    sessionula-extra = dontCheck (super.callCabal2nix "sessionula-extra" "${sessionSrc}/sessionula-extra" {});
    sessionula-file = dontCheck (super.callCabal2nix "sessionula-file" "${sessionSrc}/backend/sessionula-file" {});
    sessionula-hasql = null;
    sessionula-hedis = null;
    sessionula-wai = dontCheck (super.callCabal2nix "sessionula-wai" "${sessionSrc}/frontend/sessionula-wai" {});

    
    morpheus-graphql = self.callCabal2nix "morpheus-graphql" morpheusSrc {};
    morpheus-graphql-core = self.callCabal2nix "morpheus-graphql-core" "${morpheusSrc}/morpheus-graphql-core" {};
    morpheus-graphql-client = self.callCabal2nix "morpheus-graphql-client" "${morpheusSrcLocal}/morpheus-graphql-client" {};
  };
  
  shellToolOverrides = self: super: with pkgs; with haskell.lib; {
    ghcide = (import "${obeliskSrc}/haskell-overlays/ghcide.nix" self super).ghcide;
    haskell-ide-engine = null;
    stylish-haskell = justStaticExecutables self.stylish-haskell;
    inherit yarn;

    watchExe = (writeShellScriptBin "watchExe" '' 
      ${ghcid}/bin/ghcid -c "cabal v2-repl exe:$1" -W -T Main.main ''${@:2}
    '');
    
    watchLib = (writeShellScriptBin "watchLib" ''
      ${ghcid}/bin/ghcid -c "cabal v2-repl $1" -W ''${@:2}
    '');
    
  };
  
})
