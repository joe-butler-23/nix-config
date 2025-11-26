{pkgsUnstable, ...}: {
  home.packages = with pkgsUnstable; [
    (anki.withAddons [
      (ankiAddons.anki-connect.withConfig {
        config = {
          webCorsOriginList = [
            "http://localhost"
            "http://localhost:5173"
            "http://127.0.0.1:5173"
            "file://"
            "null"
          ];
        };
      })
    ])
  ];
}
