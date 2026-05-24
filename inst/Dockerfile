FROM ghcr.io/minotau-r/ariadne.app:latest

LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the ariadne app."

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
ADD . /app

# Make port 1234 available to the world outside this container
EXPOSE 1234

# Run the script that launches the Shiny app
CMD ["R", "-e", "library(ariadneApp); app <- shinePath(); shiny::runApp(app, port = 1234, host = '0.0.0.0')"]