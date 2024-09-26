{
  trivialBuild,
  fetchFromGitLab,
  ox-hugo,
  dash,
}:

trivialBuild rec {
  pname = "org-cv";
  version = "main";
  src = fetchFromGitLab {
    owner = "Titan-C";
    repo = "org-cv";
    rev = "dd2f840050f37cf5cb02e683f9434445288f7627";
    hash = "sha256-7YlJTQr2t1UNl1HJJxNoq8Yu8sfRxehXncH85cuPchU=";
  };
  propagatedUserEnvPkgs = [
    ox-hugo
    dash
  ];
  buildInputs = propagatedUserEnvPkgs;
}
