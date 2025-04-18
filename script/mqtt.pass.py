import os
import hashlib

def generate_salt():
    return os.urandom(16).hex()

def hash_password(password, salt):
    salted_password = salt + password
    return hashlib.sha256(salted_password.encode()).hexdigest()

salt = generate_salt()
password = "your_password"
hashed_password = hash_password(password, salt)
print(f"Salt: {salt}")
print(f"Hashed Password: {hashed_password}")


insert into mqtt_user(is_superuser, username, password_hash, salt)
values(false, 'gnss', '0a23b29a33ee51ab189b6d4e5f89808893ca365e306db92c08588d9d3523de5c', '6198f5c94e88480e4ff2327fd28a08cb');

