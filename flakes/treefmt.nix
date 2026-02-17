_: {
  perSystem =
    { ps-tools, ... }:
    {
      treefmt = {
        programs = {
          nixfmt = {
            enable = true;
            includes = [ "*.nix" ];
          };
        };
        settings.formatter.purs-tidy = {
          command = ps-tools.for-0_15.purs-tidy;
          options = [ "format-in-place" ];
          includes = [ "*.purs" ];
        };
      };
    };
}
