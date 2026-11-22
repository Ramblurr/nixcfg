package egress

import (
	"context"
	"errors"
	"log/slog"
	"net/netip"
	"strings"
	"time"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

// record is one audit line: a TCP connection, a UDP flow, a DNS query or a
// helper CONNECT.
type record struct {
	start    time.Time
	caller   *Caller
	proto    policy.Proto
	src, dst netip.AddrPort
	sni      string
	host     string
	question string
	verdict  policy.Verdict
	up, down int64
	err      error
}

// fail records a dial error. A dial refused by the guard is a deny.
func (r *record) fail(err error) {
	var de *policy.DenyError
	if errors.As(err, &de) {
		r.verdict = policy.Deny{Reason: de.Reason}
		return
	}
	r.err = err
}

func (s *Server) log(r *record) {
	logRecord(s.Audit, r)
}

func logRecord(l *slog.Logger, r *record) {
	kind, rule, reason := describe(r.verdict)
	attrs := []slog.Attr{
		slog.String("proto", string(r.proto)),
		slog.String("src", r.src.String()),
		slog.String("verdict", kind),
	}
	if r.dst.IsValid() {
		attrs = append(attrs, slog.String("dst", r.dst.String()))
	}
	if r.caller != nil {
		attrs = append(attrs, slog.String("node", r.caller.Node), slog.Any("tags", r.caller.Tags))
	}
	for _, a := range []struct{ k, v string }{
		{"sni", r.sni}, {"host", r.host}, {"question", r.question}, {"rule", rule}, {"reason", reason},
	} {
		if a.v != "" {
			attrs = append(attrs, slog.String(a.k, a.v))
		}
	}
	attrs = append(attrs,
		slog.Int64("bytes_up", r.up),
		slog.Int64("bytes_down", r.down),
		slog.Duration("duration", time.Since(r.start)),
	)
	if r.err != nil {
		attrs = append(attrs, slog.String("error", r.err.Error()))
	}
	l.LogAttrs(context.Background(), slog.LevelInfo, "flow", attrs...)
}

func describe(v policy.Verdict) (kind, rule, reason string) {
	switch v := v.(type) {
	case policy.Deny:
		return "deny", "", string(v.Reason)
	case policy.PassThrough:
		return "passthrough", v.Rule, ""
	case policy.Relay:
		return "relay", v.Rule, ""
	case policy.Decrypt:
		ids := make([]string, len(v.Rules))
		for i, r := range v.Rules {
			ids[i] = r.ID
		}
		return "decrypt", strings.Join(ids, ","), ""
	}
	return "deny", "", "undecided"
}
