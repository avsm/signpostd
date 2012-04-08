#!/usr/bin/env bash 
set -e

# ./openvpn_append_device.sh haris.d2 debian.d2 signpo.st haris.d2.signpo.st conf/ tmp/
local_node=$1
remote_node=$2
domain=$3
dst_domain=$4
conf_dir=$5
tmp_dir=$6

# create tmp folder
remote_host=$remote_node.$domain
local_host=$local_node.$domain
dst_dir=$tmp_dir/$dst_domain/

if [ ! -e $dst_dir ]; then 
  echo "Missing folder $dst_dir"
  exit 1
fi

# sign the remote domain certificate
/usr/local/bin/crypto-convert \
  -k $remote_host \
  -t DNS_PUB \
  -p $conf_dir/signpost.pem  \
  -a sign \
  -s "C=UK,O=signpost,CN=$remote_host," \
  -i "C=UK,O=signpost,CN=$local_host," \
  -T PEM_CERT \
  -D 30758400 \
  -K $dst_dir/allowed-$remote_host.crt

cat $dst_dir/tmp.crt $dst_dir/allowed-*.crt > $dst_dir/ca.crt
