#!/usr/bin/env bash 
set -e

# ./openvpn_tactic.sh 10000 0 d2.signpo.st debian haris 10.10.0.2

port=$1
dev_id=$2
domain=$3
local_node=$4
remote_node=$5
remote_ip=$6
tmp_dir=$8
conf_dir=$7

# create tmp folder
remote_host=$remote_node.$domain
local_host=$local_node.$domain
dst_dir=$tmp_dir/$remote_host/

if [ ! -e $dst_dir ]; then 
  mkdir $dst_dir
fi

# setup required key and certificates
# cp conf/signpost.pem $dst_dir/
# cp conf/signpost.crt $dst_dir/

openssl genrsa -out $dst_dir/vpn.pem 2048

# self sign key
crypto-convert \
  -k $conf_dir/signpost.pem \
  -t PEM_PRIV \
  -p $conf_dir/signpost.pem  \
  -a sign \
  -s "C=UK,O=signpost,CN=$local_host," \
  -i "C=UK,O=signpost,CN=$local_host," \
  -T PEM_CERT \
  -D 30758400 \
  -K $dst_dir/tmp.crt

# sign the vpn key
crypto-convert \
  -k $dst_dir/vpn.pem \
  -t PEM_PRIV \
  -p $conf_dir/signpost.pem  \
  -a sign \
  -s "C=UK,O=signpost,CN=vpn.$local_host," \
  -i "C=UK,O=signpost,CN=$local_host," \
  -T PEM_CERT \
  -D 30758400 \
  -K $dst_dir/vpn.crt

crypto-convert \
  -k $remote_host \
  -t DNS_PUB \
  -p $conf_dir/signpost.pem  \
  -a sign \
  -s "C=UK,O=signpost,CN=$remote_host," \
  -i "C=UK,O=signpost,CN=$local_host," \
  -T PEM_CERT \
  -D 30758400 \
  -K $dst_dir/dns.crt

cat $dst_dir/tmp.crt $dst_dir/dns.crt > $dst_dir/ca.crt
# cat $dst_dir/dns.crt > $dst_dir/ca.crt

tmp_dir=`echo $tmp_dir  | sed -e 's/\//\\\\\//g' `

cat client_tactics/openvpn/server.conf.template | sed \
   -e "s/\\\$port\\\$/$port/g" \
   -e "s/\\\$domain\\\$/$remote_host/g" \
   -e "s/\\\$tmp_dir\\\$/$tmp_dir/g" \
   -e "s/\\\$dev_id\\\$/$dev_id/g" > $dst_dir/server.conf

cat client_tactics/openvpn/client.conf.template |\
   sed -e "s/\\\$port\\\$/$port/g"\
   -e "s/\\\$dev_id\\\$/$dev_id/g" \
   -e "s/\\\$domain\\\$/$remote_host/g" \
   -e "s/\\\$tmp_dir\\\$/$tmp_dir/g" \
   -e "s/\\\$ip\\\$/$remote_ip/g" > $dst_dir/client.conf
