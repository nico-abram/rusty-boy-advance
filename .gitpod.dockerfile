FROM gitpod/workspace-full:latest

USER root
RUN apt-get update && apt-get install -y \
        libsdl2-2.0-0 \
        libsdl2-dev \
        libsdl2-image-2.0-0 \
        libsdl2-image-dev \
        libsdl2-mixer-2.0-0 \
        libsdl2-mixer-dev \
        libsdl2-ttf-2.0-0 \
        libsdl2-ttf-dev \
        libegl1-mesa-dev \
        libgles2-mesa-dev
RUN rustup default nightly