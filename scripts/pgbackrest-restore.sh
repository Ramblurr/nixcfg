#!/usr/bin/env sh
set -ex

workdir=~/work/pg
image="docker.io/library/postgres:15-bullseye"

podman pull $image

mkdir -p $workdir
cd $workdir
mkdir -p log spool
podman run --name pg-restore --userns keep-id -p 127.0.0.1:5433:5432 -v $workdir:/var/lib/postgresql/data -e POSTGRES_PASSWORD=postgres -d $image

until [ -f ./pg/global/pg_control ]; do
    sleep 1
done

sleep 1

podman stop pg-restore

rm ./pg/global/pg_control
find ./pg -mindepth 1 -delete

cat <<EOF > ./pgbackrest.conf
[global]
archive-async=y
archive-push-queue-max = 4GiB
archive-timeout = 60
compress-level = 9
compress-type = lz4
delta = y
log-path = $PWD/log
spool-path = $PWD/spool
repo1-block = y
repo1-bundle = y
repo1-path = $PWD/matrix/repo1
[db]
pg1-path = $PWD/pg
EOF

pgbackrest --config $(pwd)/pgbackrest.conf --stanza=repo1 restore
