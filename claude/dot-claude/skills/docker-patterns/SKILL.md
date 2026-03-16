---
description: Docker and Docker Compose patterns for local development, container security, networking, volume strategies, and multi-service orchestration.
---

# Docker Patterns

Docker and Docker Compose best practices for containerized development.

## When to Activate

- Setting up Docker Compose for local development
- Designing multi-container architectures
- Troubleshooting container networking or volume issues
- Reviewing Dockerfiles for security and size
- Migrating from local dev to containerized workflow

## Docker Compose for Local Development

### Standard Web App Stack

```yaml
services:
  app:
    build:
      context: .
      target: dev
    ports:
      - "3000:3000"
    volumes:
      - .:/app
      - /app/node_modules       # Anonymous volume -- preserves container deps
    environment:
      - DATABASE_URL=postgres://postgres:postgres@db:5432/app_dev
      - REDIS_URL=redis://redis:6379/0
    depends_on:
      db:
        condition: service_healthy
      redis:
        condition: service_started

  db:
    image: postgres:16-alpine
    ports:
      - "5432:5432"
    environment:
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: postgres
      POSTGRES_DB: app_dev
    volumes:
      - pgdata:/var/lib/postgresql/data
      - ./scripts/init-db.sql:/docker-entrypoint-initdb.d/init.sql
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U postgres"]
      interval: 5s
      timeout: 3s
      retries: 5

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
    volumes:
      - redisdata:/data

volumes:
  pgdata:
  redisdata:
```

### Multi-Stage Dockerfile

```dockerfile
# Stage: dependencies
FROM ruby:3.3-slim AS deps
WORKDIR /app
COPY Gemfile Gemfile.lock ./
RUN bundle install

# Stage: dev (hot reload, debug tools)
FROM ruby:3.3-slim AS dev
WORKDIR /app
COPY --from=deps /usr/local/bundle /usr/local/bundle
COPY . .
EXPOSE 3000
CMD ["bin/rails", "server", "-b", "0.0.0.0"]

# Stage: production (minimal image)
FROM ruby:3.3-slim AS production
WORKDIR /app
RUN addgroup --gid 1001 appgroup && adduser --uid 1001 --gid 1001 appuser
USER appuser
COPY --from=deps /usr/local/bundle /usr/local/bundle
COPY --chown=appuser:appgroup . .
ENV RAILS_ENV=production
EXPOSE 3000
CMD ["bin/rails", "server", "-b", "0.0.0.0"]
```

## Networking

### Service Discovery

Services in the same Compose network resolve by service name:
```
postgres://postgres:postgres@db:5432/app_dev    # "db" resolves to db container
redis://redis:6379/0                             # "redis" resolves to redis container
```

### Exposing Only What's Needed

```yaml
services:
  db:
    ports:
      - "127.0.0.1:5432:5432"   # Only accessible from host, not network
```

## Container Security

```yaml
services:
  app:
    security_opt:
      - no-new-privileges:true
    read_only: true
    tmpfs:
      - /tmp
      - /app/tmp
    cap_drop:
      - ALL
```

### Secret Management

```yaml
# GOOD: Use .env files (gitignored)
services:
  app:
    env_file:
      - .env

# BAD: Hardcoded in image
# ENV API_KEY=sk-xxxxx      # NEVER DO THIS
```

## Debugging

```bash
docker compose logs -f app           # Follow app logs
docker compose exec app sh           # Shell into app
docker compose exec db psql -U postgres  # Connect to postgres
docker compose ps                     # Running services
docker stats                          # Resource usage
docker compose up --build             # Rebuild images
docker compose down -v                # Stop + remove volumes (DESTRUCTIVE)
```

## Anti-Patterns

- Using `:latest` tag — pin to specific versions
- Running as root — always create and use a non-root user
- Storing data in containers without volumes — containers are ephemeral
- One giant container with all services — one process per container
- Putting secrets in docker-compose.yml — use .env files (gitignored)
