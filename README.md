horrible hackish code to generate migrations like:

```
ALTER TABLE table RENAME CONSTRAINT "fk_r6s9cdiav1ji7sa4gpb5dcgvo" to "fkcgb9lv2janqrrch91b0t0i8w7";
```

We use tests comparing the SQL schema generated by hibernate for our code,
and the actual schema we generate through our migrations.
When upgrading hibernate versions, it often changes the algorithm to name
constraints, forcing us to make migrations renaming them for our tests to pass.

This program takes in the schema of the DB (postgres), and the diff from this
schema to what hibernate generated (containing drop+create for the constraints),
and outputs rename constraint commands, valid for postgresql.

The advantage of having rename instead of drop+create is performance on big
databases, and obvious correctness when looking at the migration.
