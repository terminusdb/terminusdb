#!/usr/bin/env python3
from datetime import datetime, timedelta
import os
import requests

API_BASE_URL = 'https://hub.docker.com/v2'


def fetch_token():
    req = requests.post(f'{API_BASE_URL}/v2/users/login', json={
        'username': os.environ['DOCKER_USERNAME'],
        'password': os.environ['DOCKER_PASSWORD']
    }).json()
    return req['token']

def image_too_old(image: str, now: datetime) -> bool:
    date_parsed = datetime.strptime(image['tag_last_pushed'], '%Y-%m-%dT%H:%M:%S.%fZ')
    interval = now - date_parsed
    return interval.days > 14

def fetch_old_images():
    response = requests.get(f'{API_BASE_URL}/repositories/terminusdb/terminusdb-server/tags/?page_size=100&page=1').json()
    now = datetime.now()
    return [x['name'] for x in response['results'] if x['name'].startswith("dev-") and image_too_old(x, now)]

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
