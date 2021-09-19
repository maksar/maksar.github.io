---
title: Migrating to the nix flakes for macbook setup
tags: nix
language: english
---

For the long time, I've been using [nix-darwin](@gh(LnL7):nix-darwin) and [home-manager](@gh(nix-community):home-manager) to configure and and keep my laptop configuration up-to-date. Today I had a little bit of spare time and decided to finally migrate towards using nix flakes.

<!--more-->

There are several reasons:

- Purity. Being pure, flakes are forcing you to keep external dependencies in the outer layer (in `flake.nix` file).
- Speed. Without `fetchGit` in random places, evaluation (and activation) times are much smaller.
- Control. Since there are no channels, your setup is easier to reproduce.

## Purity

nix flakes forbids your derivations to depend on local state (file on local filesystem). Due to that limitation, you are forced to:

- have everything nix needs in git
- have all your dependencies to be publicly available in git

You can no longer `import` a file from your local filesystem (or a predefined channel), which increases reproducibility and robustness. As an example: I had such `neovim` setup beforehand:

```nix
{ config, pkgs, ... }:
 let
  envy = builtins.fetchGit {
    url = "https://github.com/Shados/envy";
    ref = "master";
  };

in {
  imports = [ (import "${envy}/home-manager.nix" { }) ];
  ...
}
```

Since `builtins.fetchGit` isn't allowed in nix flakes, I was forced to convert [envy](@gh(Shados:envy)) to the dependency of my configuration flake:

```nix
inputs = {
  ...
  envy = { url = "github:Shados/envy"; flake = false; };
};

...

homeManagerModules = {
  vim-envy = import "${envy}/home-manager.nix" {};
};
```

As a result, there is no need for `darwin-rebuild` to fetch a master branch from [envy](@gh(Shados:envy)) each time I do `switch` operation.

## Control

Since there are no dependencies for the local state, it is quite possible to check the consistency of the whole configuration on Github CI.

```sh
# Build and switch to bootstrap config
nix build .#darwinConfigurations.bootstrap.system
./result/sw/bin/darwin-rebuild switch --flake .#bootstrap
# Build and switch to full config
/run/current-system/sw/bin/zsh -c './result/sw/bin/darwin-rebuild switch --flake .#githubCI'
```

Furthermore, Github CI can perform dependencies update for me every week `nix flake update` and even push the results to the [cachix](https://app.cachix.org/cache/maksar).

```yml
- name: If scheduled, update inputs
  if: ${{ github.event_name == 'schedule' && success() }}
  run: |
    nix flake update
...

- name: If scheduled, push commit with updated inputs
  if: ${{ github.event_name == 'schedule' && success() }}
  run: |
    git config user.name github-actions
    git config user.email github-actions@github.com
    git add .
    git commit "Update inputs"
    git push
```

My nix configuration, which was inspired by excellent [repo](@gh(malob):dotfiles) of [Malo Bourgon](@t:m_bourgon), is available [here](@gh(maksar):dotfiles).

## Speed

This one is pretty much speaks for itself. Whole configuration switch, with all [homebrew](https://brew.sh) update cycle (yeah, not everything is still in nix) takes up to 13 seconds!

```sh
$ time darwin-rebuild switch --flake .#macbook

building the system configuration...
user defaults...
setting up user launchd services...
Homebrew bundle...
Using homebrew/cask
Using homebrew/cask-versions
Using homebrew/cask-drivers
Using microsoft/mssql-release
Using rkaippully/tools
Using unixodbc
Using msodbcsql17
Using nmap
Using gamgee
Using mas
Using logitech-options
Using skype
Using slack
Using viber
Using telegram
Using zoom
Using microsoft-teams
Using miro
Using obs
Using dropbox
Using 1password
Using alfred
Using unshaky
Using openvpn-connect
Using firefox
Using google-chrome
Using vivaldi
Using safari-technology-preview
Using transmission
Using tunnelbear
Using docker
Using sourcetree
Using charles
Using virtualbox
Using jetbrains-toolbox
Using typora
Using steam
Using vlc
Using postman
Using Amphetamine
Using Excel
Using Pages
Using PowerPoint
Using RemoteDesktop
Using Word
Using XCode
Homebrew Bundle complete! 46 Brewfile dependencies now installed.
setting up pam...
setting up groups...
setting up users...
setting up ~/Applications...
applying patches...
setting up /etc...
system defaults...
setting up launchd services...
configuring networking...
configuring fonts...
Activating home-manager configuration for maksar
Starting home manager activation
Activating checkFilesChanged
Activating checkLinkTargets
Activating writeBoundary
Activating copyFonts
Activating installPackages
replacing old 'home-manager-path'
installing 'home-manager-path'
Activating linkGeneration
Cleaning up orphan links from /Users/maksar
No change so reusing latest profile generation 230
Creating home file links in /Users/maksar
Activating onFilesChange

4.10s user 4.16s system 64% cpu 12.800 total
```

## Linux

After a while, I extended configuration to have a Linux support also:

Adding following lines to the `flake.nix` file:

```nix
# Build and activate with `nix build .#cloudVM.activationPackage; ./result/activate`
cloudVM = home-manager.lib.homeManagerConfiguration {
  system = "x86_64-linux";
  stateVersion = "21.05";
  homeDirectory = "/home/maksar";
  username = "maksar";
  configuration = {
    os = "linux";
    imports = [ homeManagerCommonConfig ];
    nixpkgs = nixpkgsConfig;
  };
};
```

Allows to configure by linux boxes in absolutely the same way as I do on my macbook.

```sh
nix build .#cloudVM.activationPackage
./result/activate
```

Wonderful `zsh-powerlevel10k` nix package allows my terminal (`kitty`) to look awesome in any OS

<a href="/images/flakes/darwin.png" class="fresco" data-fresco-group="thumbnail" data-fresco-options="ui: 'inside', thumbnails: false"><img src="/previews/flakes/darwin.png"/></a>
<a href="/images/flakes/linux.png" class="fresco" data-fresco-group="thumbnail" data-fresco-options="ui: 'inside', thumbnails: false"><img src="/previews/flakes/linux.png"/></a>
