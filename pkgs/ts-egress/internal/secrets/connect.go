package secrets

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"net/http"
	"net/url"
	"regexp"
	"strings"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

// Connect reads fields from a 1Password Connect server over its REST API.
// Vault and item may be IDs or titles; a field matches by ID, or else, unless
// the reference is FieldByID, by a label no other field in scope shares; a
// section, when given, matches the same way and narrows the scope to its
// fields. This follows iron-proxy's
// 1password_connect source (internal/transform/secrets/op_connect_resolver.go,
// Apache-2.0, see NOTICE) without the Connect SDK.
type Connect struct {
	Host   string // e.g. http://op-connect:8080
	Token  string
	Client *http.Client // nil means http.DefaultClient
}

// opID is the shape of a 1Password ID; anything else is looked up by title.
var opID = regexp.MustCompile(`^[a-z0-9]{26}$`)

func (c *Connect) Fetch(ctx context.Context, ref policy.OPRef) (string, error) {
	vault, err := c.lookup(ctx, "/v1/vaults", ref.Vault)
	if err != nil {
		return "", fmt.Errorf("vault %q: %w", ref.Vault, err)
	}
	items := "/v1/vaults/" + url.PathEscape(vault) + "/items"
	item, err := c.lookup(ctx, items, ref.Item)
	if err != nil {
		return "", fmt.Errorf("item %q: %w", ref.Item, err)
	}
	var full connectItem
	if err := c.get(ctx, items+"/"+url.PathEscape(item), &full); err != nil {
		return "", fmt.Errorf("item %q: %w", ref.Item, err)
	}
	return full.field(ref)
}

// lookup returns the ID of the one vault or item at path titled ref, as
// connect-sdk-go looks them up.
func (c *Connect) lookup(ctx context.Context, path, ref string) (string, error) {
	if opID.MatchString(ref) {
		return ref, nil
	}
	var found []struct {
		ID string `json:"id"`
	}
	q := url.Values{"filter": {fmt.Sprintf("title eq %q", ref)}}
	if err := c.get(ctx, path+"?"+q.Encode(), &found); err != nil {
		return "", err
	}
	if len(found) != 1 {
		return "", fmt.Errorf("%d matches", len(found))
	}
	return found[0].ID, nil
}

func (c *Connect) get(ctx context.Context, path string, out any) error {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, strings.TrimSuffix(c.Host, "/")+path, nil)
	if err != nil {
		return err
	}
	req.Header.Set("Authorization", "Bearer "+c.Token)
	client := c.Client
	if client == nil {
		client = http.DefaultClient
	}
	resp, err := client.Do(req)
	if err != nil {
		return requestError(err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("connect answered %d", resp.StatusCode)
	}
	if err := json.NewDecoder(resp.Body).Decode(out); err != nil {
		return errors.New("connect sent an unreadable response")
	}
	return nil
}

// requestError keeps only the class of a failed request: transport errors
// can quote the server's bytes, and errors end up in the audit log.
func requestError(err error) error {
	var op *net.OpError
	switch {
	case errors.Is(err, context.DeadlineExceeded), errors.Is(err, context.Canceled):
		return errors.New("connect timed out")
	case errors.As(err, &op) && op.Op == "dial":
		return errors.New("connect is unreachable")
	}
	return errors.New("connect request failed")
}

type connectItem struct {
	Fields []struct {
		ID      string `json:"id"`
		Label   string `json:"label"`
		Value   string `json:"value"`
		Section *struct {
			ID string `json:"id"`
		} `json:"section"`
	} `json:"fields"`
	Sections []struct {
		ID    string `json:"id"`
		Label string `json:"label"`
	} `json:"sections"`
}

// field returns the value ref names. A section, when ref has one, is
// resolved first, and only its fields are in scope. In both steps an exact
// ID wins, and otherwise the label must name exactly one candidate.
func (it *connectItem) field(ref policy.OPRef) (string, error) {
	section, err := it.section(ref.Section)
	if err != nil {
		return "", err
	}
	var byLabel []string
	for _, f := range it.Fields {
		if ref.Section != "" && (f.Section == nil || f.Section.ID != section) {
			continue
		}
		if f.ID == ref.Field {
			return f.Value, nil
		}
		if f.Label == ref.Field && !ref.FieldByID {
			byLabel = append(byLabel, f.Value)
		}
	}
	switch len(byLabel) {
	case 0:
		return "", fmt.Errorf("field %q not found", ref.Field)
	case 1:
		return byLabel[0], nil
	}
	return "", fmt.Errorf("field %q is ambiguous: %d fields have that label", ref.Field, len(byLabel))
}

// section returns the ID of the section ref names, or "" for no section.
func (it *connectItem) section(ref string) (string, error) {
	if ref == "" {
		return "", nil
	}
	var byLabel []string
	for _, s := range it.Sections {
		if s.ID == ref {
			return s.ID, nil
		}
		if s.Label == ref {
			byLabel = append(byLabel, s.ID)
		}
	}
	switch len(byLabel) {
	case 0:
		return "", fmt.Errorf("section %q not found", ref)
	case 1:
		return byLabel[0], nil
	}
	return "", fmt.Errorf("section %q is ambiguous: %d sections have that label", ref, len(byLabel))
}
