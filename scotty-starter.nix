{ mkDerivation, aeson, base, blaze-html, blaze-markup, clay, scotty
, stdenv, text, wai-extra, wai-middleware-static, xml-conduit
}:
mkDerivation {
  pname = "app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html blaze-markup clay scotty text wai-extra
    wai-middleware-static xml-conduit
  ];
  homepage = "https://github.com/JonathanReeve/tei-server";
  description = "A simple JSON REST server for TEI XML documents.";
  license = stdenv.lib.licenses.gpl3;
  hydraPlatforms = stdenv.lib.platforms.none;
}
