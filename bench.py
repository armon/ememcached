import time
import memcache
import random
import multiprocessing

MIN_KEY_SIZE = 8
MAX_KEY_SIZE = 250
NUM_KEYS = 1024
READ_PCT = 0.90
RUNS = 1000000
WORKERS = 4

# Data in sizes of 256B, 1K, 4K, 32K and 64K
DATA_BLOCKS = ["FOO!"*64,"BAR!"*256] #, "BAZ!"*1024, "ZIP!"*8096, "ZIM!"*16384]

def generate_keys():
    key_space = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_."
    keys = []
    for k in xrange(NUM_KEYS):
        key_len = int(random.uniform(MIN_KEY_SIZE, MAX_KEY_SIZE))
        key = "".join(random.choice(key_space) for i in xrange(key_len))
        keys.append(key)
    return keys

def run_loop(client, keys):
    read_time = 0
    write_time = 0
    total_time = 0
    for i in xrange(RUNS):
        # Check for read/write
        read = random.random() <= READ_PCT
        key = random.choice(keys)

        start = time.time()
        if read:
            client.get(key)
        else:
            val = random.choice(DATA_BLOCKS)
            client.set(key, val)
        end = time.time()

        if read:
            read_time += end-start
        else:
            write_time += end-start
        total_time += end-start

        if i % 10000 == 0:
            print "(%d/%d) Read: %f Write: %f Total: %f, %f/sec" % (i+1, RUNS, read_time, write_time, total_time, (i+1)/total_time)

def worker(keys):
    client = memcache.Client(("localhost:11211",))
    run_loop(client, keys)

def main():
    keys = generate_keys()
    workers = []
    for x in xrange(WORKERS):
        t = multiprocessing.Process(target=worker, args=(keys,))
        t.start()
        workers.append(t)

    [w.join() for w in workers]


if __name__ == "__main__":
    main()

