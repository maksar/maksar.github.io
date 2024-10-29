---
title: wget and read-only file system don't play well together
tags: ECS, AWS
language: english
---

Short story about _how I spent good 2 hours of yesterday’s evening_.

We use `wget` utility as a health-check probe pretty much in every ecs container.

```hcl
  healthcheck = {
    command     = ["CMD-SHELL", "wget --no-verbose --tries=1 --spider http://localhost:8000/health || exit 1"]
    timeout     = 30
    interval    = 10
    retries     = 3
    startPeriod = 10
  }
```

Very handy as `wget` exists everywhere. It even existed in google-provided [vertex AI](https://cloud.google.com/vertex-ai) predictor images.

```sh
  docker pull us-docker.pkg.dev/vertex-ai/automl-tabular/prediction-server:20240609_1425
```

So I just re-used that piece of container configuration. Annoyingly ECS kept marking containers as **unhealthy** soon after start despite all my efforts to fine-tune health-check delay/retry parameters. Worst of all – I can clearly see probing http requests in container logs and `200 OK` response statuses.

It is impossible to see logs of a health-check procedure anywhere in ECS _afair_. After scratching my head for a bit, I replaced `wget` with `curl` as official `ECS` [help page](https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_HealthCheck.html) suggests and it worked! Containers are **healthy** and I was able to finish my day.

But subtle background thought of “why” never left me… Today in the morning's shower it finally clicked – readonly filesystem! When I was playing with vertex predictor I noticed that it didn’t like `HEAD` http requests, only `GET /health` was good enough for it to answer with `200 OK` response code (otherwise it gave nasty `405 Method Not Allowed`), so I removed `--spider` argument to `wget`. What `wget` does by default? Right, it saves the downloaded file to a current folder…

Container definition looks something like that:

```hcl
  module "vertex_container_definition" {
    source    = "cloudposse/ecs-container-definition/aws"
    version   = "~>0.58.1"
    essential = true
    command   = ["bash", "-c", "python3 -m google3.third_party.py.cloud_ml_autoflow.prediction.launcher"]

    readonly_root_filesystem = true
    mount_points             = [
      {
        sourceVolume  = "tmp"
        readOnly      = false
        containerPath = "/tmp"
      }
    ]
  }
```

`wget` was sending requests, but wasn't able to save downloaded content to a disk, that caused health-check command to emit `1` exit code and `ECS` marked container as **unhealthy**.
