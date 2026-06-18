# Legacy SAS -> Python Module Port

This project reimplements a small SAS-style edit-check and summary routine in Python and validates parity behavior with `pytest` on synthetic data.

## Business Context

Legacy analytics pipelines often rely on SAS programs that enforce row-level data quality checks before reporting. This project demonstrates how to port that logic into a clean Python module without changing expected rule behavior.

## What This Port Covers

- **Row-level edit checks** for missing/invalid values.
- **Deterministic output ordering** to make comparisons stable.
- **Summary counts by check** for reporting parity with legacy-style outputs.
- **Pytest parity tests** over synthetic datasets (including edge cases).

## Project Structure

```text
Legacy_SAS_Python_Module_Port/
├── src/
│   ├── __init__.py
│   └── port.py
├── tests/
│   └── test_port_parity.py
└── requirements.txt
```

## Setup

From repository root:

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r Legacy_SAS_Python_Module_Port/requirements.txt
```

## Run Tests

```bash
pytest Legacy_SAS_Python_Module_Port/tests -q
```

Expected result: all tests pass, confirming parity-style outputs for flagged rows and check-level summaries.

## Why This Is Interview-Ready

- Shows practical migration from legacy SAS workflow to modular Python.
- Demonstrates reproducible quality controls with automated tests.
- Makes translation choices explicit and discussable in a technical interview.
