---
title: Creating shell completions and man pages with opt-env-conf
tags: haskell, nix, Cabal
series: haskell-nix
language: english
---

Wonderful [opt-env-conf](https://hackage.haskell.org/package/opt-env-conf) package provides a way to generate nice-looking man pages and also shell completion scripts.

However, there was a slight problem – my program have some colors in `--help` output. That made both shell completions and man pages to look a bit ugly, because raw coloring escape sequences were there.

I didn't want to touch contents of the script (it might change in future versions), so I decided to strip escape sequences from the program output with `.completion-helper-${exeName}` shim.

```nix
installManpage = exeName: drv: hlib.overrideCabal drv (old: {
  postInstall = (old.postInstall or "") + ''
    mkdir -p $out/share/man/man1/
    export NO_COLOR=1
    $out/bin/${exeName} --render-man-page | ${pkgs.colorized-logs}/bin/ansi2txt > ${exeName}.1
    ${pkgs.gzip}/bin/gzip -9 -c ${exeName}.1 > $out/share/man/man1/${exeName}.1.gz
  '';
});
installManpages = exeNames: drv: lib.foldr installManpage drv exeNames;

installCompletion = exeName: drv: hlib.overrideCabal drv (old: {
  postInstall = (old.postInstall or "") + ''
    bashCompDir=$out/share/bash-completion/completions
    zshCompDir=$out/share/zsh/vendor-completions
    fishCompDir=$out/share/fish/vendor_completions.d
    mkdir -p $bashCompDir $zshCompDir $fishCompDir

    completionHelper=$out/bin/.completion-helper-${exeName}

    echo "#!${pkgs.bash}/bin/bash" > $completionHelper
    echo "$out/bin/${exeName} \$@ | ${pkgs.colorized-logs}/bin/ansi2txt" >> $completionHelper
    chmod +x $completionHelper

    $out/bin/${exeName} --bash-completion-script $completionHelper > $bashCompDir/${exeName}
    $out/bin/${exeName} --zsh-completion-script $completionHelper > $zshCompDir/_${exeName}
    $out/bin/${exeName} --fish-completion-script $completionHelper > $fishCompDir/${exeName}.fish
  '';
});
installCompletions = exeNames: drv: lib.foldr installCompletion drv exeNames;
```

To enable everything in one go, you can wrap your binary with `installManpagesAndCompletions` combinator.

```nix
installManpagesAndCompletions = exeNames: drv: installManpages exeNames (installCompletions exeNames drv);

haskellOverlay = hfinal: hprev: {
  project = installManpagesAndCompletions ["project-bin"] (hfinal.callCabal2nix "project" ./. { });
};
```
