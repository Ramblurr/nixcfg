#!/usr/bin/env python3
import getpass
from passlib.hash import sha512_crypt


password = getpass.getpass("Enter password: ")

# Hash passwords using SHA-512
h = sha512_crypt.hash(password)


print(f"password_hash: {h}")
