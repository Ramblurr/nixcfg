// Package broker asks the credential broker which credential a caller may
// use. It speaks newline-delimited JSON over the broker's unix socket: one
// decide request and one answer per connection. Every answer is checked
// here, so the egress acts only on a well-formed Verdict.
package broker

import (
	"bufio"
	"cmp"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"regexp"
	"strings"
	"time"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

// Client dials Socket for each decision. The broker records every allow as
// a use, so decisions are never cached.
type Client struct {
	Socket string
	// Timeout bounds one decision, dial included. Zero means two seconds.
	Timeout time.Duration
}

// Request is one decide call. Identity is the caller's canonical identity,
// Groups and Modes its broker ceiling. Header is the canonical name of the
// header that carries the placeholder.
type Request struct {
	Identity   string
	Node       string
	Groups     []string
	Modes      []policy.BrokerMode
	Credential string
	Host       string
	Header     string
	Mode       policy.BrokerMode
}

// Verdict is one of Allow, Pending or Deny.
type Verdict interface{ verdict() }

// Allow carries the credential to inject. Approval is empty for an
// assignment that needs none.
type Allow struct {
	Approval   string
	Credential policy.Credential
}

// Pending means a human has to approve the use first.
type Pending struct{ Approval string }

// Deny carries the broker's reason.
type Deny struct{ Reason string }

func (Allow) verdict()   {}
func (Pending) verdict() {}
func (Deny) verdict()    {}

type wireRequest struct {
	Op         string              `json:"op"`
	Identity   string              `json:"identity"`
	Node       string              `json:"node"`
	Groups     []string            `json:"groups"`
	Modes      []policy.BrokerMode `json:"modes"`
	Credential string              `json:"credential"`
	Host       string              `json:"host"`
	Header     string              `json:"header"`
	Mode       policy.BrokerMode   `json:"mode"`
}

type wireResponse struct {
	Decision string `json:"decision"`
	Approval string `json:"approval"`
	Reason   string `json:"reason"`
	Secret   *struct {
		Vault string `json:"vault"`
		Item  string `json:"item"`
		Field string `json:"field"`
	} `json:"secret"`
	Inject *struct {
		Header string `json:"header"`
	} `json:"inject"`
}

const maxResponse = 64 << 10

// Decide asks the broker about req. Any error means the broker could not
// decide, and the caller must refuse.
func (c *Client) Decide(ctx context.Context, req Request) (Verdict, error) {
	ctx, cancel := context.WithTimeout(ctx, cmp.Or(c.Timeout, 2*time.Second))
	defer cancel()
	conn, err := (&net.Dialer{}).DialContext(ctx, "unix", c.Socket)
	if err != nil {
		return nil, err
	}
	defer conn.Close()
	dl, _ := ctx.Deadline()
	_ = conn.SetDeadline(dl)
	w := wireRequest{
		Op: "decide", Identity: req.Identity, Node: req.Node, Groups: req.Groups, Modes: req.Modes,
		Credential: req.Credential, Host: req.Host, Header: req.Header, Mode: req.Mode,
	}
	if w.Groups == nil {
		w.Groups = []string{}
	}
	if w.Modes == nil {
		w.Modes = []policy.BrokerMode{}
	}
	line, err := json.Marshal(w)
	if err != nil {
		return nil, err
	}
	if _, err := conn.Write(append(line, '\n')); err != nil {
		return nil, err
	}
	r := bufio.NewReaderSize(conn, maxResponse)
	answer, err := r.ReadSlice('\n')
	if err != nil {
		return nil, fmt.Errorf("reading answer: %w", err)
	}
	var resp wireResponse
	if err := json.Unmarshal(answer, &resp); err != nil {
		return nil, fmt.Errorf("answer: %w", err)
	}
	return resp.verdict(req.Credential)
}

var approvalRE = regexp.MustCompile(`^[A-Za-z0-9._:-]{1,128}$`)

func (w wireResponse) verdict(name string) (Verdict, error) {
	approval := w.Approval
	if approval != "" && !approvalRE.MatchString(approval) {
		return nil, fmt.Errorf("invalid approval id %q", approval)
	}
	switch w.Decision {
	case "allow":
		if w.Secret == nil || w.Inject == nil {
			return nil, errors.New("allow without secret or inject")
		}
		for _, part := range []string{w.Secret.Vault, w.Secret.Item, w.Secret.Field} {
			if part == "" || strings.Contains(part, "/") {
				return nil, errors.New("secret vault, item and field must be non-empty and hold no /")
			}
		}
		op := policy.OPRef{Vault: w.Secret.Vault, Item: w.Secret.Item, Field: w.Secret.Field, FieldByID: true}
		secret := policy.SecretRef{Scheme: policy.SecretOnePassword, Ref: "op://" + op.Vault + "/" + op.Item + "/" + op.Field, OP: op}
		cred, err := policy.NewCredential(name, secret, w.Inject.Header, "{secret}")
		if err != nil {
			return nil, err
		}
		return Allow{Approval: approval, Credential: cred}, nil
	case "pending":
		if approval == "" {
			return nil, errors.New("pending without approval id")
		}
		return Pending{Approval: approval}, nil
	case "deny":
		return Deny{Reason: w.Reason}, nil
	}
	return nil, fmt.Errorf("unknown decision %q", w.Decision)
}
