# Define variables
DOCKER = docker
IMAGE_BASE = censusaggregatorapp_base
IMAGE_LATEST = censusaggregatorapp:latest
IMAGE_REMOTE = purposeanalytics/censusaggregatorapp:uat
BUILD_DIR = ../deploy
CHROME_LINE = "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main"

# Default target
all: update-dockerfile build_base build_latest tag push

# Update the Dockerfile to install Google Chrome
update-dockerfile:
	@if ! grep -q "$(CHROME_LINE)" $(BUILD_DIR)/Dockerfile_base; then \
	sed -i '/^FROM/a \
	# Install dependencies\n\
	RUN apt-get update && apt-get install -y wget gnupg\n\
	\n\
	# Install Google Chrome\n\
	RUN wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | apt-key add - \\\n\
	    && echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list \\\n\
	    && apt-get update \\\n\
	    && apt-get install -y google-chrome-stable\n\
	' $(BUILD_DIR)/Dockerfile_base; \
	fi
	sed -i 's/RUN R -e '\''install.packages("remotes")'\''/RUN R -e '\''install.packages(c("remotes", "ggspatial"))'\''/' $(BUILD_DIR)/Dockerfile_base
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
