.PHONY: brain-sync brain-check brain-simulate literature-check maintenance-check parity-check

brain-sync:
	bash Infrastructure/scripts/manage_generated_copies.sh sync

brain-check:
	bash Infrastructure/scripts/manage_generated_copies.sh check

brain-simulate:
	bash Infrastructure/scripts/simulate_cps_review_smoke_test.sh

literature-check:
	python3 Infrastructure/scripts/validate_literature_catalog.py

# parity-check verifies the structural relationships between canonical
# Infrastructure files and the generated adapter trees. It complements
# brain-check (which catches content drift) by catching structural gaps:
# missing adapter mirrors, orphan adapter files, uncovered metadata, and
# unapproved canonical-source duplications.
parity-check:
	python3 Infrastructure/scripts/check_skill_parity.py
	python3 Infrastructure/scripts/check_adapter_metadata_coverage.py
	python3 Infrastructure/scripts/check_canonical_consistency.py

# maintenance-check delegates orchestration to a shell script that runs
# every underlying check, continues past failures, prints a summary, and
# exits non-zero if any check failed. Standalone targets above remain
# fail-fast — they're meant for debugging a single check in isolation.
maintenance-check:
	bash Infrastructure/scripts/run_maintenance_checks.sh
