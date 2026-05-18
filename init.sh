#!/usr/bin/env bash
set -euo pipefail

echo "Refreshing keys..."
pacman-key --refresh-keys || true

echo "Disabling sandbox..."
sed -i 's/^#DisableSandbox/DisableSandbox/' /etc/pacman.conf

echo "Running ldd on clasp..."
ldd "$(command -v clasp)"

echo "Installing qlot..."
curl -fsSL https://qlot.tech/installer | sh

echo "Running qlot init..."
qlot init