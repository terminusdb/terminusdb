#!/usr/bin/env python3
import os
import requests

API_BASE_URL = 'https://hub.docker.com/v2'


def fetch_token():
    req = requests.post(f'{API_BASE_URL}/v2/users/login', json={
        'username': os.environ['DOCKER_USERNAME'],
        'password': os.environ['DOCKER_PASSWORD']
    }).json()
    return req['token']


def fetch_old_images():
    response = requests.get(f'{API_BASE_URL}/repositories/terminusdb/terminusdb-server/tags/?page_size=100&page=1').json()
    return [x['name'] for x in response['results'] if x['name'].startswith("dev-")]

def remove_old_images(token: str, old_images: [str]):
    bearer_headers = {'Authorization': f'Bearer {token}'}
    for image in old_images:
        requests.delete(f'{API_BASE_URL}/repositories/terminusdb/terminusdb-server/tags/{image}', headers=bearer_headers)

def main():
    token = fetch_token()
    old_images = fetch_old_images()
    remove_old_images(token, old_images)
    print(old_images)

if __name__ == '__main__':
    main()
