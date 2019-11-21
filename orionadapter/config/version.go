// nolint:lll
// Generate our adapter's resource yaml. To do this, you actually have to run
// `scripts/gen-config.sh`, read the comments there!!
//go:generate $REPO_ROOT/bin/mixer_codegen.sh -a mixer/adapter/orionadapter/config/config.proto -x "-s=false -n orionadapter -t authorization"

package config

// Version of this adapter.
var Version = "0.1.0"
