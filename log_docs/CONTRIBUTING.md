# Contributing to Zapier Triggers API

Welcome! This monorepo contains multiple implementations of the Zapier Triggers API, along with a unified test suite for comparing them.

## Repository Structure

```
zapier/
├── zapier_python/          # Python (FastAPI) implementation
├── zapier_elixir/          # Elixir (Phoenix) implementation
├── zapier_rust/            # Rust implementation (WIP)
├── unified_test_suite/     # Cross-implementation test framework
├── scripts/                # Helper scripts
├── COMPARISON_SUMMARY.md   # Performance comparison results
└── project_spec.md         # Original requirements
```

## Getting Started

### Prerequisites

Depending on which implementation you're working with:

**Python:**
- Python 3.12+
- [UV package manager](https://github.com/astral-sh/uv)
- PostgreSQL 16+
- Redis 7+

**Elixir:**
- Elixir 1.14+
- Erlang/OTP 25+
- PostgreSQL 16+

**Rust:**
- Rust 1.70+ (via rustup)
- PostgreSQL 16+

### Setup

Use the convenience scripts:

```bash
# Setup specific implementation
./scripts/setup-python.sh
./scripts/setup-elixir.sh
./scripts/setup-rust.sh

# Start all implementations
./scripts/start-all.sh

# Run all tests
./scripts/test-all.sh
```

Or manually:

```bash
# Python
cd zapier_python
uv sync
uv run uvicorn src.zapier_triggers_api.main:app --reload

# Elixir
cd zapier_elixir/zapier_triggers
mix deps.get
mix ecto.create && mix ecto.migrate
mix phx.server

# Rust
cd zapier_rust
cargo build
cargo run
```

## Development Workflow

### 1. Creating a New Feature

When adding a feature to one or more implementations:

1. **Review the spec**: Check `project_spec.md` for requirements
2. **Check existing implementations**: See how others solved it
3. **Write tests first**: Add tests to `unified_test_suite/` if cross-cutting
4. **Implement**: Work in the specific implementation directory
5. **Test**: Run implementation tests + unified test suite
6. **Document**: Update relevant README files

### 2. Making Changes

**Branch naming:**
- `feat/description` - New features
- `fix/description` - Bug fixes
- `perf/description` - Performance improvements
- `docs/description` - Documentation only
- `test/description` - Test additions/fixes

**Commit messages:**
```
type(scope): brief description

Longer description if needed

Examples:
- feat(python): add event deduplication
- fix(elixir): correct rate limiting logic
- perf(unified-tests): optimize benchmark runs
- docs(readme): update setup instructions
```

### 3. Testing

**Run tests before committing:**

```bash
# Test everything
./scripts/test-all.sh

# Test specific implementation
cd zapier_python && uv run pytest
cd zapier_elixir/zapier_triggers && mix test

# Run unified test suite
cd unified_test_suite && ./run_tests.sh --type functional
```

**Add tests for new features:**
- Implementation-specific: Add to that impl's test directory
- Cross-cutting: Add to `unified_test_suite/tests/test_functional.py`

### 4. Code Quality

**Python:**
```bash
cd zapier_python
uv run ruff check .        # Linting
uv run ruff format .       # Formatting
uv run mypy .              # Type checking
```

**Elixir:**
```bash
cd zapier_elixir/zapier_triggers
mix format                 # Formatting
mix credo                  # Code analysis
mix dialyzer               # Type checking
```

**Rust:**
```bash
cd zapier_rust
cargo fmt                  # Formatting
cargo clippy               # Linting
cargo test                 # Tests
```

## Pull Request Process

1. **Create feature branch** from `main`
2. **Make changes** following guidelines above
3. **Run all tests** with `./scripts/test-all.sh`
4. **Update documentation** if needed
5. **Submit PR** with clear description:
   - What changed
   - Why it changed
   - Which implementation(s) affected
   - Test coverage
6. **Address review feedback**
7. **Squash and merge** when approved

### PR Checklist

- [ ] Tests pass (`./scripts/test-all.sh`)
- [ ] Code is formatted/linted
- [ ] Documentation updated
- [ ] No secrets or credentials committed
- [ ] Commit messages follow convention
- [ ] Changes tested locally

## Implementation Guidelines

### Python (FastAPI)

- Use `async`/`await` for I/O operations
- Type hints required (enforced by mypy)
- Follow PEP 8 style guide
- Use SQLModel for database models
- Keep dependencies minimal

### Elixir (Phoenix)

- Follow Elixir formatting (`mix format`)
- Use pattern matching and guards
- Leverage OTP behaviors (GenServer, etc.)
- Write comprehensive doctests
- Use Ecto for database operations

### Rust

- Follow Rust idioms and conventions
- Use `cargo fmt` and `cargo clippy`
- Write comprehensive tests
- Document public APIs
- Handle errors explicitly (no panics in production code)

## Adding to Unified Test Suite

When adding tests that should run across implementations:

1. **Location**: `unified_test_suite/tests/test_functional.py`
2. **Use fixtures**: `any_client` runs test on both Python + Elixir
3. **Mark appropriately**: `@pytest.mark.functional`
4. **Skip gracefully**: Use `pytest.skip()` for implementation-specific tests
5. **Document**: Add docstring explaining what's tested

Example:
```python
@pytest.mark.functional
def test_new_feature(any_client: APIClient):
    """Test that new feature works correctly."""
    if any_client.implementation == "python":
        pytest.skip("Feature not in Python yet")

    # Test code here
    result = any_client.some_new_method()
    assert result.status_code == 200
```

## Performance Testing

Run benchmarks before/after performance changes:

```bash
cd unified_test_suite
./run_tests.sh --type performance
```

Compare results in `reports/` directory.

## Documentation

Update docs when:
- Adding new features
- Changing API behavior
- Modifying setup process
- Finding bugs or gotchas

**Files to update:**
- Implementation README: `zapier_{lang}/README.md`
- Root README: `/README.md` (if major changes)
- API docs: `zapier_elixir/.../API_DOCUMENTATION.md`
- This file: If workflow changes

## Questions?

- Check existing implementations for examples
- Review `COMPARISON_SUMMARY.md` for implementation differences
- Open an issue for clarification
- Ask in PR comments

## License

By contributing, you agree that your contributions will be licensed under the same license as the project.
