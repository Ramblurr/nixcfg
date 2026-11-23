package main

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"slices"
	"strings"
	"testing"
	"time"

	"tailscale.com/tailcfg"
	"tailscale.com/tstest/integration/testcontrol"
	"tailscale.com/types/logger"
)

func TestTagsFlag(t *testing.T) {
	for _, tt := range []struct {
		value   string
		want    []string
		wantErr string
	}{
		{"tag:moot-wright", []string{"tag:moot-wright"}, ""},
		{"tag:moot-wright,tag:moot-workstead", []string{"tag:moot-wright", "tag:moot-workstead"}, ""},
		{"", nil, ""},
		{"moot-wright", nil, "must start with 'tag:'"},
		{"tag:moot-wright, tag:moot-workstead", nil, "must start with 'tag:'"},
		{"tag:moot-wright,", nil, "must start with 'tag:'"},
		{"tag:", nil, "must not be empty"},
		{"tag:1moot", nil, "must start with a letter"},
		{"tag:moot_wright", nil, "only contain numbers, letters, or dashes"},
	} {
		t.Run(tt.value, func(t *testing.T) {
			tags := tagsFlag{"tag:previous"}
			err := tags.Set(tt.value)
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("Set(%q) error = %v, want one containing %q", tt.value, err, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("Set(%q): %v", tt.value, err)
			}
			if !slices.Equal(tags, tt.want) {
				t.Errorf("Set(%q) = %q, want %q", tt.value, tags, tt.want)
			}
			if got := tags.String(); got != tt.value {
				t.Errorf("String() = %q, want %q", got, tt.value)
			}
		})
	}
}

type keyRequest struct {
	Capabilities struct {
		Devices struct {
			Create struct {
				Reusable      bool     `json:"reusable"`
				Ephemeral     bool     `json:"ephemeral"`
				Preauthorized bool     `json:"preauthorized"`
				Tags          []string `json:"tags"`
			} `json:"create"`
		} `json:"devices"`
	} `json:"capabilities"`
}

func startOAuthAPI(t *testing.T, mintedKey string) (string, <-chan keyRequest) {
	requests := make(chan keyRequest, 4)
	api := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		switch r.URL.Path {
		case "/api/v2/oauth/token":
			io.WriteString(w, `{"access_token":"oauth-token","token_type":"Bearer"}`)
		case "/api/v2/tailnet/-/keys":
			if r.Header.Get("Authorization") != "Bearer oauth-token" {
				http.Error(w, "unauthorized", http.StatusUnauthorized)
				return
			}
			var req keyRequest
			if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
				http.Error(w, err.Error(), http.StatusBadRequest)
				return
			}
			requests <- req
			json.NewEncoder(w).Encode(map[string]string{"key": mintedKey})
		default:
			http.NotFound(w, r)
		}
	}))
	t.Cleanup(api.Close)
	return api.URL, requests
}

func TestOAuthClientSecretLogin(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	const mintedKey = "tskey-auth-minted"
	apiURL, requests := startOAuthAPI(t, mintedKey)
	control := &testcontrol.Server{
		DERPMap:        startDERP(t),
		DNSConfig:      &tailcfg.DNSConfig{Proxied: true},
		MagicDNSDomain: "tail-scale.ts.net",
		Logf:           logger.Discard,
		RequireAuthKey: mintedKey,
		TagOwners:      map[string][]string{"tag:moot-wright": nil, "tag:moot-workstead": nil},
	}
	control.HTTPTestServer = httptest.NewServer(control)
	t.Cleanup(control.HTTPTestServer.Close)

	for _, tt := range []struct {
		name          string
		attrs         string
		tags          string
		wantErr       string
		wantEphemeral bool
		wantPreauth   bool
	}{
		{name: "without tags", tags: "", wantErr: "require --advertise-tags"},
		{name: "defaults", tags: "tag:moot-wright", wantEphemeral: true},
		{name: "persistent", attrs: "&ephemeral=false&preauthorized=true", tags: "tag:moot-wright,tag:moot-workstead", wantPreauth: true},
	} {
		t.Run(tt.name, func(t *testing.T) {
			t.Setenv("TS_AUTHKEY", "tskey-client-k123-secret?baseURL="+apiURL+tt.attrs)
			var tags tagsFlag
			if err := tags.Set(tt.tags); err != nil {
				t.Fatal(err)
			}
			s := newServer("workstead", t.TempDir(), tags)
			s.ControlURL = control.HTTPTestServer.URL
			s.Logf = logger.Discard
			t.Cleanup(func() { s.Close() })

			st, err := s.Up(ctx)
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("Up error = %v, want one containing %q", err, tt.wantErr)
				}
				select {
				case req := <-requests:
					t.Fatalf("minted a key without tags: %+v", req)
				default:
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}

			create := (<-requests).Capabilities.Devices.Create
			if !slices.Equal(create.Tags, tags) || create.Reusable || create.Ephemeral != tt.wantEphemeral || create.Preauthorized != tt.wantPreauth {
				t.Errorf("minted key capabilities = %+v, want tags %q, single use, ephemeral %v, preauthorized %v",
					create, tags, tt.wantEphemeral, tt.wantPreauth)
			}
			if got := st.Self.Tags.AsSlice(); !slices.Equal(got, slices.Sorted(slices.Values(tags))) {
				t.Errorf("node tags = %q, want %q", got, tags)
			}
		})
	}
}

func TestChildEnvStripsAuthKey(t *testing.T) {
	got := childEnv([]string{
		"PATH=/bin",
		"TS_AUTHKEY=tskey-client-secret",
		"TS_AUTH_KEY=tskey-auth-alias",
		"TS_CLIENT_SECRET=tskey-client-alias",
		"HOME=/tmp",
	})
	want := []string{"PATH=/bin", "HOME=/tmp", "TSPLUG_ACTIVE=1"}
	if !slices.Equal(got, want) {
		t.Fatalf("childEnv() = %q, want %q", got, want)
	}
}
