{
  dotnetCorePackages,
  bicep,
  buildDotnetModule,
}:

let
  self = buildDotnetModule {
    inherit (bicep)
      src
      version
      postPatch
      dotnet-runtime
      doCheck
      nativeCheckInputs
      ;

    dotnet-sdk = dotnetCorePackages.sdk_8_0;

    nugetDeps = ./deps.nix;

    pname = "bicep-langserver";
    projectFile = "src/Bicep.LangServer/Bicep.LangServer.csproj";
    testProjectFile = "src/Bicep.LangServer.UnitTests/Bicep.LangServer.UnitTests.csproj";
  };
in
self
