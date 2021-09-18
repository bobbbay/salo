{ buildIdris, idris2, fetchFromGitHub, readline }:

buildIdris {
  name = "effect";
  version = "0.0.0";

  src = fetchFromGitHub {
    owner = "russoul";
    repo = "idris2-effect";
    rev = "8d0f5381454931e94c855ebc11253e0bcfd5b051";
    sha256 = "sha256-Uz3UybSOLetaa46kDKoh0SDxhPJOLX05orrlySEVNck=";
  };
}
