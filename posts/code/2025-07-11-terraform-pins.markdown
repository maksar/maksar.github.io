---
title: Terraform and nix integration
tags: terraform, AWS, nix
language: english
---

There are some deficiencies related to `terraform` from the point of view of a `nix` user.

<!--more-->

- ### No centralized providers version management

Each `terraform` project uses own lock file and therefore own versions of the providers. This might be a good thing to have if your sub-projects are dis-connected from each other and managed by different people/teams. But in my case, all of the sub-projects (in infrastructure monorepo) better to evolve almost simultaneously. After Updating provider in one of them, we'd really want to pay our taxes and also migrate others. But this becomes tedious work if versions are specified directly in `.tf` files. You have to update all of them one-by-one, re-init and apply. I'd prefer a centralized version management of `terraform` providers.

- ### No centralized external modules version management

Same problems as with providers, really. We had many places in `terraform` code, where same module is used, but with different versions. Contracts (inputs variables of the module) do change from version to version, sometimes there are breaking changed introduced. It is hard to follow the stream of changes and reason about what version supports what set of variables. Again, it would be great to have central place to say: "I want to use particular version of `s3-bucket` module". And no, relying on the **latest** isn't a good idea as there are no guarantees what _latest_ really means in each particular case -- no one is interested to inspect `.terraform/modules/modules.json` file.

- ### Installation procedure requires internet connection

With a huge bandwidth... Even single `aws` provider takes **731 Mb**. Then there are `google` with **120 Mb**, `grafana` with **78 Mb**, etc. Yes, I'm aware about `TF_PLUGIN_CACHE_DIR` trick and we actually used it before. But it doesn't play nice with `terraform init -update`, it also has no effect if you erase `.terraform.lock.hcl` and decide to re-initialize (indeed, how `terraform` would know which version to use).

## Research

The solution I came up with was inspired by two things:

- _native_ `terraform` [support](@gh(NixOS):nixpkgs/blob/release-25.05/pkgs/applications/networking/cluster/terraform/default.nix#L101) in `nix`
- [nixpkgs-terraform-providers-bin](@gh(nix-community):nixpkgs-terraform-providers-bin) repository

There is a way to setup `terraform.withPlugins`, where you can pre-select all of the plugins you are going to use. After entering `nix shell`, all of them will be _seen_ by `terraform`. Terraform doesn't support such feature out of the box, but thankfully, `nix` has a [patch](@gh(NixOS):nixpkgs/blob/release-25.05/pkgs/applications/networking/cluster/terraform/provider-path-0_15.patch) to support mentioned behavior. Of course, `nix` also sets [NIX_TERRAFORM_PLUGIN_DIR](@gh(NixOS):nixpkgs/blob/release-25.05/pkgs/applications/networking/cluster/terraform/default.nix#L177) environment variable to be always seen by `terraform`.

Unfortunately, [list](@gh(NixOS):nixpkgs/blob/release-25.05/pkgs/applications/networking/cluster/terraform-providers/providers.json) of providers in `nix` can sometimes be outdated and [update script](@gh(NixOS):nixpkgs/blob/release-25.05/pkgs/applications/networking/cluster/terraform-providers/update-provider) can't pin to a specific version, only to the latest. Further more, update script was build to update list of provider versions _inside_ `nixpkgs`, it is un-ergonomic to maintain onw list of providers in the source code repository as [derivation builder](@gh(NixOS):nixpkgs/blob/release-25.05/pkgs/applications/networking/cluster/terraform-providers/default.nix#L18) isn't exposed.

[nixpkgs-terraform-providers-bin](@gh(nix-community):nixpkgs-terraform-providers-bin) repository supposed to maintain more _recent_ versions of providers with [cron script](@gh(nix-community):nixpkgs-terraform-providers-bin/blob/master/.github/workflows/cron.yml). But what if you want to pin some particular provider version to use to not be a latest?.. So I decided to make my own version of it.

## Solution

First, I create a specification `./terraform/versions.json` file with list of all providers and modules that are going to be used. Exact versions used here were initially extracted from terraform code (and lock files).

```json
{
  "modules": {
    "terraform-aws-modules/terraform-aws-alb": "v8.7.0",
    "terraform-aws-modules/terraform-aws-ec2-instance": "v6.0.2",
    "terraform-aws-modules/terraform-aws-ecs": "v4.1.1",
    "terraform-aws-modules/terraform-aws-lambda": "v8.0.1",
    "tweag/terraform-nixos": "646cacb12439ca477c05315a7bfd49e9832bc4e3",
    "terraform-aws-modules/terraform-aws-rds-aurora": "v8.4.0",
    "terraform-aws-modules/terraform-aws-s3-bucket": "v5.2.0",
    "terraform-aws-modules/terraform-aws-step-functions": "v3.0.0",
    "terraform-aws-modules/terraform-aws-vpc": "v5.5.3"
  },
  "providers": {
    "change-engine/slack-app": "0.1.2",
    "change-engine/slack-token": "0.1.4",
    "cloudposse/template": "2.2.0",
    "cloudposse/utils": "1.30.0",
    "figma/slack": "1.3.2",
    "gitlabhq/gitlab": "18.1.1",
    "grafana/grafana": "3.25.7",
    "hashicorp/archive": "2.7.1",
    "hashicorp/aws": "6.3.0",
    "hashicorp/awscc": "1.49.0",
    "hashicorp/external": "2.3.5",
    "hashicorp/google": "6.43.0",
    "hashicorp/http": "3.5.0",
    "hashicorp/local": "2.5.3",
    "hashicorp/null": "3.2.4",
    "hashicorp/random": "3.7.2",
    "hashicorp/tls": "4.1.0",
    "kreuzwerker/docker": "3.6.2",
    "metio/git": "2025.6.20",
    "salrashid123/http-full": "1.3.1",
    "scottwinkler/shell": "1.7.10",
    "tailscale/tailscale": "0.21.1",
    "TheNicholi/json-formatter": "0.1.1"
  }
}
```

Then, there is a small `ruby` script to fetch the exact download urls and `sha265` hashes out of the internet for `nix` to consume later:

```ruby
#! /usr/bin/env nix-shell
#! nix-shell -i ruby -p 'ruby.withPackages (p: [p.parallel])' nix-prefetch-github

require 'json'
require 'net/http'
require 'open-uri'
require 'fileutils'
require 'rubygems'
require 'parallel'

def provider_get(path)
  JSON.parse URI.open("https://registry.terraform.io/v1/providers/#{path}").read
end

def module_get(path)
  JSON.parse URI.open("https://registry.terraform.io/v1/modules/#{path}").read
end

def get_provider_version(owner, repo, version, os, arch)
  data = provider_get("#{owner}/#{repo}/#{version}/download/#{os}/#{arch}")
  { sha256: data["shasum"], url: data["download_url"] }
end

def update_provider(name, version)
  owner, repo = name.split("/")
  versions = provider_get("#{owner}/#{repo}/versions")["versions"]
  version_data = versions.filter { _1["version"] =~ /^[\d\.]+$/ }.detect { _1["version"] == version }
  platforms = version_data["platforms"].map.with_object({}) do |data, sum|
    arch = data["arch"]
    os = data["os"]
    sum["#{os}_#{arch}"] = get_provider_version(owner, repo, version, os, arch) if os == "linux" && (arch == "arm64" || arch == "amd64")
  end

  { platforms: platforms.sort_by(&:first).to_h, source: { owner: owner, repo: repo, version: version } }.tap do |d|
    latest_version = versions.filter { _1["version"] =~ /^[\d\.]+$/ }.map { _1["version"] }.sort_by { Gem::Version.new(_1) }.last
    puts "#{owner}/#{repo} => #{version} (#{"but #{latest_version} " if version != latest_version}is the latest)"
  end
end

def update_module(name, version)
  owner, repo = name.split("/")
  JSON.parse(`nix-prefetch-github #{owner} #{repo} --rev #{version}`).tap do |data|
    latest_version = module_get("#{owner}/#{repo.sub("terraform-aws-", "")}")["modules"].first["tag"] rescue "unknown"
    puts "#{owner}/#{repo} => #{version} (#{"but #{latest_version} " if version != latest_version}is the latest)"
  end
end

input = JSON.parse(File.read("terraform/versions.json"))

File.write("terraform/versions.lock.json", JSON.pretty_generate(
  providers: Hash[Parallel.map(input["providers"]) { |name, version| [name, update_provider(name, version)] }],
  modules: Hash[Parallel.map(input["modules"]) { |name, version| [name, update_module(name, version)] }]
))
```

Upon execution, it prints out versions it fetched, suggests newer version to use and generates `./terraform/versions.lock.json` lock file with all information required for nix.

```text
figma/slack => 1.3.2 (is the latest)
change-engine/slack-token => 0.1.4 (is the latest)
change-engine/slack-app => 0.1.2 (is the latest)
cloudposse/template => 2.2.0 (is the latest)
gitlabhq/gitlab => 18.1.1 (is the latest)
grafana/grafana => 3.25.7 (is the latest)
hashicorp/archive => 2.7.1 (is the latest)
cloudposse/utils => 1.30.0 (is the latest)
hashicorp/http => 3.5.0 (is the latest)
hashicorp/external => 2.3.5 (is the latest)
hashicorp/local => 2.5.3 (is the latest)
hashicorp/null => 3.2.4 (is the latest)
hashicorp/google => 6.43.0 (is the latest)
hashicorp/awscc => 1.49.0 (is the latest)
hashicorp/aws => 6.3.0 (is the latest)
hashicorp/tls => 4.1.0 (is the latest)
hashicorp/random => 3.7.2 (is the latest)
kreuzwerker/docker => 3.6.2 (is the latest)
salrashid123/http-full => 1.3.1 (is the latest)
scottwinkler/shell => 1.7.10 (is the latest)
tailscale/tailscale => 0.21.1 (is the latest)
TheNicholi/json-formatter => 0.1.1 (is the latest)
metio/git => 2025.6.20 (is the latest)
tweag/terraform-nixos => 646cacb12439ca477c05315a7bfd49e9832bc4e3 (but unknown is the latest)
terraform-aws-modules/terraform-aws-s3-bucket => v5.2.0 (is the latest)
terraform-aws-modules/terraform-aws-lambda => v8.0.1 (is the latest)
terraform-aws-modules/terraform-aws-step-functions => v3.0.0 (but v5.0.1 is the latest)
terraform-aws-modules/terraform-aws-ec2-instance => v6.0.2 (is the latest)
terraform-aws-modules/terraform-aws-ecs => v4.1.1 (but v6.0.5 is the latest)
terraform-aws-modules/terraform-aws-alb => v8.7.0 (but v9.17.0 is the latest)
terraform-aws-modules/terraform-aws-rds-aurora => v8.4.0 (but v9.15.0 is the latest)
terraform-aws-modules/terraform-aws-vpc => v5.5.3 (but v6.0.1 is the latest)
```

With `./terraform/versions.lock.json` lock file in place, we can load it to `nix` and convert to the list of derivations of modules and providers:

```nix
versions = importJSON ./terraform/versions.lock.json;

terraformModules = pkgs.linkFarm "modules" (
  mapAttrsToList (name: path: { inherit name path; }) (mapAttrs (trivial.const pkgs.fetchFromGitHub) versions.modules)
);

terraformProviders = pkgs.symlinkJoin { name = "providers"; paths = concatLists (
  mapAttrsToList (name: provider:
    mapAttrsToList (mkTerraformProvider provider.source) provider.platforms
  ) versions.providers);
};

mkTerraformProvider = makeOverridable ({ owner, repo, version }:
  platform: url:
  pkgs.stdenv.mkDerivation {
    pname = "terraform-provider-${repo}-${platform}";
    version = version;
    src = pkgs.fetchurl url;
    unpackPhase = "${pkgs.unzip}/bin/unzip -o $src";
    buildPhase = ":";
    dontPatchELF = true;
    dontStrip = true;
    dontPatchShebangs = true;
    installPhase = ''
      dir=$out/libexec/terraform-providers/registry.terraform.io/${owner}/${repo}/${version}/${platform}
      mkdir -p "$dir"
      mv terraform-* "$dir/"
    '';
  });
```

It is important to use the following spell to forbid `nix` from modifying binaries in the `$out` directory in any way. This preserves downloaded binaries and it is guaranteed to have the same have (being built with any value of `system` in nix):

```nix
dontPatchELF = true;
dontStrip = true;
dontPatchShebangs = true;
```

Unfortunately, stock `terraform.withPlugins` has a major flaw, as it creates an [extra wrapper](@gh(NixOS):nixpkgs/blob/release-25.05/pkgs/applications/networking/cluster/terraform/default.nix#L167-L170) script. Well, with that wrapper terraform **works**, providers are recognized and being installed correctly, but `terraform` uses the wrapper file itself to calculate the hash of the provider (and put into `.terraform.lock.hcl` file). That leads to hard-to-debug behavior on different platforms.

You install and lock a provider for both `amd64` and `arm64` on your `x86-64_linux` machine, only to realize that hashes of the providers doesn't match on your CICD server, which is evaluated on `aarch64-linux` machine. All makes sense, since derivations in `nix` are architecture-dependant and a link inside a wrapper script would be different for each nix `system`.

Lets write own `terraform` wrapper. This is pretty simple with handy `pkgs.symlinkJoin` builder:

```nix
terraformWithProviders = pkgs.symlinkJoin {
  name = "terraform-with-providers";
  paths = [ pkgs.terraform ];
  nativeBuildInputs = [ pkgs.makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/terraform --set NIX_TERRAFORM_PLUGIN_DIR ${terraformProviders}/libexec/terraform-providers
  '';
};
```

Finally, `terraformWithProviders` and `terraformModules` are mentioned in `devShell` section of the `flake.nix` file:

```nix
devShell = pkgs.mkShell {
  buildInputs = with pkgs; [
    terraformWithProviders
  ];

  shellHook = ''
    rm -rf terraform/modules
    ln -sf ${terraformModules} terraform/modules
  '';
};
```

Now, it should be possible to use any of the installed providers and modules in the `terraform` code:

```hcl
provider "slack" {
  token = data.aws_secretsmanager_secret_version.god.secret_string
}

provider "gitlab" {
  token = data.aws_secretsmanager_secret_version.gitlab_token.secret_string
}

module "some_bucket" {
  source = "../terraform/modules/terraform-aws-modules/terraform-aws-s3-bucket"
}
```

After executing `./terraform/update.rb`, it is trivial (and fast, no internet connection required) to re-init `terraform` project:

```sh
rm -rf .terraform.lock.hcl
rm -rf .terraform
terraform init
terraform providers lock -platform=linux_arm64 -platform=linux_amd64 -fs-mirror=$(cat .terraform/plugin_path | jq -r '.[0]')
```
