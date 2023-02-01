# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.222.0/containers/ubuntu/.devcontainer/base.Dockerfile

ARG VARIANT="hirsute"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

RUN apt-get update \
    && apt-get -y install --no-install-recommends software-properties-common

RUN sudo apt-add-repository -y -u ppa:swi-prolog/stable
RUN sudo apt-get install -y swi-prolog
USER vscode
RUN swipl -g "pack_install( log4p,[interactive(false)] ), halt"