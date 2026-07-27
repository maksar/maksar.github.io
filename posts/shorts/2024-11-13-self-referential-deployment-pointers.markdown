---
title: Self-referential deployment pointers
tags: terraform, AWS
language: english
---

Here is cool trick you can do in `Terraform` in order to _remember_ last version of the application you deployed.

Common way to do it is to declare a variable

First of all, you need an variable to read version from
```hcl
variable "release_version" {
  type    = string
  default = null
}
```

And then have a `tfvars` file in the source code repository, which will point to the _current_ version of the application:

```hcl
release_version = "0cf879d3809e428787f35ace54b45c6f"
```

Having that in place is convenient for many reasons:
- it is immediately obvious which version is currently deployed and who performed the deploy (`git` log holds the history)
- repeated invocations of `terraform apply` are idempotent, no need to worry about "forgetting" to specify `-var` cli argument

But there is a different way, which might be more convenient in case you want to deploy very often, in automated way (from CI, without human interaction).

Declare an `output`, which captures a version of the application after deploy:

```hcl
module "application" {
  source = "../modules/application"
  image_tag = coalesce(var.release_version, data.terraform_remote_state.self.outputs.deployed_release_version)
}

output "release_version" {
  value = module.application.tagged_image.source_tag
}
```

And a `terraform_remote_state` data block, which points to the terraform state of the project itself (hence the `self` name).

```hcl
data "terraform_remote_state" "self" {
  backend = "s3"
  config = {
    bucket = "terraform-state"
    key    = "key.tfstate"
  }
}
```

The magic sauce is this line, which tries to read from input variable, and, if absent, from the output of `self` state.

```hcl
coalesce(var.release_version, data.terraform_remote_state.self.outputs.deployed_release_version)
```

So we have idempotency for free (it is safe to invoke `terraform apply` without parameters) and ability to release a new version by providing a `-var=release_version=new_sha` cli argument to `terraform apply`