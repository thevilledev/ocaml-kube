# Security policy

This project has not made its first stable release. Security fixes will be
applied to the latest development branch until a supported-release policy is
announced.

Please report vulnerabilities privately to `ville@vesilehto.fi`. Include the
affected revision, impact, reproduction details, and any suggested mitigation.
Do not open a public issue before coordinated disclosure.

The client executes kubeconfig credential commands when the selected user entry
contains an `exec` configuration. A kubeconfig is therefore executable input and
must only be loaded from a trusted source. The client never invokes a shell on
its own; the configured command and argument vector are passed directly to the
operating system.
