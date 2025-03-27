# Define variables
DOCKER = docker
IMAGE_BASE = censusaggregatorapp_base
IMAGE_LATEST = censusaggregatorapp:latest
IMAGE_REMOTE = purposeanalytics/censusaggregatorapp:uat
BUILD_DIR = ../deploy

# Default target
all: build_base build_latest tag push

# Build the base image
build_base:
	cd $(BUILD_DIR) && $(DOCKER) build -f Dockerfile_base --progress=plain -t $(IMAGE_BASE) .

# Build the latest image
build_latest:
	cd $(BUILD_DIR) && $(DOCKER) build -f Dockerfile --progress=plain -t $(IMAGE_LATEST) .

# Tag the latest image
tag:
	cd $(BUILD_DIR) && $(DOCKER) tag $(IMAGE_LATEST) $(IMAGE_REMOTE)

# Push the latest image to the remote repository
push:
	cd $(BUILD_DIR) && $(DOCKER) push $(IMAGE_REMOTE)

# Clean up (optional, if you have cleanup commands)
clean:
	# Add any cleanup commands here if needed
