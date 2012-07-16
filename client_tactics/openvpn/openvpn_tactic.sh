#!/usr/bin/env bash 
set -e

# ./openvpn_tactic.sh 10000 0 d2.signpo.st debian haris 10.10.0.2

port=$1
dev_id=$2
domain=$3
local_node=$4
remote_node=$5
remote_ip=$6
dst_domain=$7
tmp_dir=$9
conf_dir=$8

# create tmp folder
remote_host=$remote_node.$domain
local_host=$local_node.$domain
dst_dir=$tmp_dir/$dst_domain/

if [ ! -e $dst_dir ]; then 
  mkdir $dst_dir
fi

openssl genrsa -out $dst_dir/vpn.pem 2048

# self sign key
/usr/local/bin/crypto-convert \
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
/usr/local/bin/crypto-convert \
  -k $dst_dir/vpn.pem \
  -t PEM_PRIV \
  -p $conf_dir/signpost.pem  \
  -a sign \
  -s "C=UK,O=signpost,CN=vpn.$local_host," \
  -i "C=UK,O=signpost,CN=$local_host," \
  -T PEM_CERT \
  -D 30758400 \
  -K $dst_dir/vpn.crt

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
# cat $dst_dir/dns.crt > $dst_dir/ca.crt

tmp_dir=`echo $tmp_dir  | sed -e 's/\//\\\\\//g' `

cat $conf_dir/../client_tactics/openvpn/server.conf.template | sed \
   -e "s/\\\$port\\\$/$port/g" \
   -e "s/\\\$domain\\\$/$dst_domain/g" \
   -e "s/\\\$tmp_dir\\\$/$tmp_dir/g" \
   -e "s/\\\$dev_id\\\$/$dev_id/g" > $dst_dir/server.conf

cat $conf_dir/../client_tactics/openvpn/client.conf.template |\
   sed -e "s/\\\$port\\\$/$port/g"\
   -e "s/\\\$dev_id\\\$/$dev_id/g" \
   -e "s/\\\$domain\\\$/$dst_domain/g" \
   -e "s/\\\$tmp_dir\\\$/$tmp_dir/g" \
   -e "s/\\\$ip\\\$/$remote_ip/g" > $dst_dir/client.conf

chmod a+x $dst_dir
chmod -R a+rw $dst_dir
