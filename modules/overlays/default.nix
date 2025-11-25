{inputs}: _final: prev: {
  opencode = inputs.opencode.packages.${prev.system}.default;
}
