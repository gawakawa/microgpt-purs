_: {
  perSystem =
    { config, ... }:
    {
      apps.default = {
        type = "app";
        program = "${config.packages.default}/bin/microgpt";
      };
    };
}
