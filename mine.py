import urllib.request
import urllib.error
import time
import json

hosts = [
    "http://localhost:8080",
    "http://localhost:8081",
    "http://localhost:8082",
    "http://localhost:8083",
    "http://localhost:8084",
    "http://localhost:8085",
    "http://localhost:8086",
    "http://localhost:8087",
]

def request(url, data=None):
    try:
        response = urllib.request.urlopen(url, data.encode('utf-8'))
        print(response.read().decode('utf-8'))
    except urllib.error.HTTPError:
        pass

def transaction_pool(node):
    response = urllib.request.urlopen(f'{node}/transaction_pool')
    return json.load(response)

for i in range(len(hosts) - 1):
    request(f'{hosts[i]}/add_peer', f'host=127.0.0.1&port={3000 + i + 1}')

node_idx = 0
time_since_last_block_mined = 10

while True:
    # Running a round robin in order to keep mining the blocks
    time.sleep(1)

    node = hosts[node_idx % len(hosts)]
    node_idx += 1


    pool = transaction_pool(node)
    if len(pool) == 0 and time_since_last_block_mined < 10:
        time_since_last_block_mined += 1
        continue

    request(f'{node}/mine_block', '')
    time_since_last_block_mined = 0
