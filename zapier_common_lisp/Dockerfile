FROM clfoundation/sbcl:latest

WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libev-dev \
    postgresql-client \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp \
         --eval "(quicklisp-quickstart:install :path \"/root/quicklisp/\")" \
         --eval "(ql:add-to-init-file)" \
         --eval "(quit)" && \
    rm quicklisp.lisp

# Copy application
COPY . /app/

# Load dependencies
RUN sbcl --eval "(ql:quickload :zapier-triggers)" --quit

# Expose port
EXPOSE 5000

# Set environment
ENV PORT=5000
ENV WORKER_COUNT=4
ENV ENVIRONMENT=production
ENV DATABASE_URL=postgresql://postgres:postgres@localhost:5432/zapier_triggers

# Start server
CMD ["sbcl", "--eval", "(ql:quickload :zapier-triggers :silent t)", \
     "--eval", "(zapier-triggers:main)"]
