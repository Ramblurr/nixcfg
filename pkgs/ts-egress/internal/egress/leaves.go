// Copyright 2025 The iron-proxy Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Adapted from github.com/ironsh/iron-proxy internal/certcache/cache.go
// at commit c32e47d653256ad7b1638214f60b5859ff94b268. Changes: a bounded
// map instead of golang-lru, PEM input instead of file paths, and leaves
// renewed an hour before they expire.

package egress

import (
	"crypto"
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/tls"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/pem"
	"errors"
	"fmt"
	"math/big"
	"sync"
	"time"
)

const (
	leafLifetime = 24 * time.Hour
	leafRenew    = time.Hour
	maxLeaves    = 1024
)

// Leaves mints and caches TLS leaf certificates signed by the egress CA.
type Leaves struct {
	ca  *x509.Certificate
	key crypto.Signer

	mu    sync.Mutex
	cache map[string]*tls.Certificate
}

// NewLeaves parses a PEM CA certificate and its PEM private key (PKCS#8,
// PKCS#1 or SEC 1).
func NewLeaves(certPEM, keyPEM []byte) (*Leaves, error) {
	block, _ := pem.Decode(certPEM)
	if block == nil {
		return nil, errors.New("CA certificate: no PEM block")
	}
	ca, err := x509.ParseCertificate(block.Bytes)
	if err != nil {
		return nil, fmt.Errorf("CA certificate: %w", err)
	}
	if !ca.IsCA || ca.KeyUsage&x509.KeyUsageCertSign == 0 {
		return nil, errors.New("CA certificate cannot sign certificates")
	}
	kb, _ := pem.Decode(keyPEM)
	if kb == nil {
		return nil, errors.New("CA key: no PEM block")
	}
	var parsed any
	if parsed, err = x509.ParsePKCS8PrivateKey(kb.Bytes); err != nil {
		if parsed, err = x509.ParseECPrivateKey(kb.Bytes); err != nil {
			if parsed, err = x509.ParsePKCS1PrivateKey(kb.Bytes); err != nil {
				return nil, errors.New("CA key: not PKCS#8, SEC 1 or PKCS#1")
			}
		}
	}
	key, ok := parsed.(crypto.Signer)
	if !ok {
		return nil, errors.New("CA key cannot sign")
	}
	if pub, ok := key.Public().(interface{ Equal(crypto.PublicKey) bool }); !ok || !pub.Equal(ca.PublicKey) {
		return nil, errors.New("CA key does not match the certificate")
	}
	return &Leaves{ca: ca, key: key, cache: map[string]*tls.Certificate{}}, nil
}

// For returns a leaf for name, minting one when none is cached or the
// cached one is within leafRenew of expiring.
func (l *Leaves) For(name string) (*tls.Certificate, error) {
	l.mu.Lock()
	defer l.mu.Unlock()
	if c, ok := l.cache[name]; ok && time.Now().Before(c.Leaf.NotAfter.Add(-leafRenew)) {
		return c, nil
	}
	c, err := l.mint(name)
	if err != nil {
		return nil, fmt.Errorf("minting a leaf for %s: %w", name, err)
	}
	if len(l.cache) >= maxLeaves {
		clear(l.cache)
	}
	l.cache[name] = c
	return c, nil
}

func (l *Leaves) mint(name string) (*tls.Certificate, error) {
	key, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	if err != nil {
		return nil, err
	}
	serial, err := rand.Int(rand.Reader, new(big.Int).Lsh(big.NewInt(1), 128))
	if err != nil {
		return nil, err
	}
	now := time.Now()
	tmpl := &x509.Certificate{
		SerialNumber: serial,
		Subject:      pkix.Name{CommonName: name},
		DNSNames:     []string{name},
		NotBefore:    now.Add(-time.Minute),
		NotAfter:     now.Add(leafLifetime),
		KeyUsage:     x509.KeyUsageDigitalSignature,
		ExtKeyUsage:  []x509.ExtKeyUsage{x509.ExtKeyUsageServerAuth},
	}
	der, err := x509.CreateCertificate(rand.Reader, tmpl, l.ca, &key.PublicKey, l.key)
	if err != nil {
		return nil, err
	}
	leaf, err := x509.ParseCertificate(der)
	if err != nil {
		return nil, err
	}
	return &tls.Certificate{Certificate: [][]byte{der, l.ca.Raw}, PrivateKey: key, Leaf: leaf}, nil
}
