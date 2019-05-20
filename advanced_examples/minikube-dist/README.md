# Using Minikube and Erlang

This is a quick demo of using minikube to run a distributed Erlang application.
The example we will use is the
[Docker Watch](http://github.com/erlang/docker-erlang-example/tree/master)
node as a base.

This is only meant to be an example of how to get started. It is not the only,
nor neccesarily the best way to setup minikube with distributed Erlang.

# Other Demos

* [Using Docker](http://github.com/erlang/docker-erlang-example/)
* [Using Docker: Logstash](http://github.com/erlang/docker-erlang-example/tree/logstash)
* [Using Docker Compose: Logstash/ElasticSearch/Kibana](http://github.com/erlang/docker-erlang-example/tree/elk)
* [Using Minikube: Simple](http://github.com/erlang/docker-erlang-example/tree/minikube-simple)
* [Using Minikube: Prometheus/Grafana](http://github.com/erlang/docker-erlang-example/tree/minikube-prom-graf)
* [Using Minikube: Distributed Erlang](http://github.com/erlang/docker-erlang-example/tree/minikube-dist)
* [Using Minikube: Encrypted Distributed Erlang](http://github.com/erlang/docker-erlang-example/tree/minikube-tls-dist)

# Prerequisites

To start with you should familiarize yourself with minikube through this guide:
https://kubernetes.io/docs/setup/minikube/

In a nutshell:

## Install

 * [VirtualBox](https://www.virtualbox.org/wiki/Downloads)
 * [kubectl](https://kubernetes.io/docs/tasks/tools/install-kubectl/)
 * [minikube](https://github.com/kubernetes/minikube/releases)

## Start and test

    > minikube start
    > kubectl run hello-minikube --image=k8s.gcr.io/echoserver:1.10 --port=8080
    > kubectl expose deployment hello-minikube --type=NodePort
    > curl $(minikube service hello-minikube --url)
    ## Should print a lot of text
    > kubectl delete services hello-minikube
    > kubectl delete deployment hello-minikube
    > minikube stop

# Deploying Dockerwatch

In this demo we will be doing three things:

* Create a new application called backend for mnesia
* Modify dockerwatch to use mnesia as its storage
* Create a Service and Deployment for the backend
* Create a Deployment of dockerwatch that implements the Service

First however, make sure that the minikube cluster is started:

    > minikube start

and that you have cloned this repo and checked out this branch:

    > git clone https://github.com/erlang/docker-erlang-example
    > cd docker-erlang-example
    > git checkout minikube-dist

## Create backend

The purpose of the backend is to be the service responsible for writing
keeping the data. So the only thing it needs to do is some mnesia
initialization when [starting](backend/src/backend_app.erl).

Since we are running inside a kluster where each pod gets its own IP address
we don't really need epmd any more. So in this example we use the `-epmd_module`
to implement our own static epmd client that always returns port 12345 as the
distribution port. The module looks like this:

```
-module(epmd_static).

-export([start_link/0, register_node/2, register_node/3,
         port_please/2, address_please/3]).
%% API.

start_link() ->
    ignore.

register_node(Name, Port) ->
    register_node(Name, Port, inet_tcp).
register_node(_Name, _Port, _Driver) ->
    {ok, 0}.

port_please(_Name, _Host) ->
    {port, 12345, 5}.

address_please(Name, Host, AddressFamily) ->
    erl_epmd:address_please(Name, Host, AddressFamily).
```

The module is then configured to be used in [vm.args.src](backend/config/vm.args.src):

    -start_epmd false
    -epmd_module epmd_static

Lastly net_kernel has to be configured to listen to the port, this is done in the
[sys.config.src](backend/config/sys.config.src):

```
{kernel, [{logger,[{handler,default,logger_std_h,#{}}]},
          %%,{logger_level,info}
           {inet_dist_listen_min, 12345},
           {inet_dist_listen_max, 12345}
          ]},
```

## Modify dockerwatch

The modification of the dockerwatch module to use mnesia is pretty straight forward.
You can see the end result [here](dockerwatch/src/dockerwatch.erl).

The same modifications with distribution port has to be done for dockerwatch as well.
As we want to be able to scale the number of dockerwatch frontends as load increases
we need to use a node name that is unique in the cluster. Using the IP address of
the pod is a simple solution that works for now.

In order to get the IP the following config has to be added to the dockerwatch deployment
config:

```
      env:
        - name: IP
          valueFrom:
            fieldRef:
              fieldPath: status.podIP
```

and then we just use `${IP}` in [vm.args.src](dockerwatch/config/vm.args.src):

    -name dockerwatch@${IP}

Also mnesia is configured to connect to the backend service at startup in
[sys.config.src](dockerwatch/config/sys.config.src).

```
{mnesia, [{extra_db_nodes,['dockerwatch@backend.default.svc.cluster.local']}]}
```

## Deploy the backend
    
We first setup the backend node as a service in order to easily find how to connect with it
from the dockerwatch nodes. This only works if there is only one backend node. If you
want to add more you need to use a more complex solution.

```
kubectl create service clusterip backend --tcp=12345:12345
service/backend created
```

The backend is build like this:

    eval $(minikube docker-env)
    docker build -t backend -f Dockerfile.backend .

and then deployed like this:

```
cat <<EOF | kubectl apply -f -
apiVersion: apps/v1
kind: Deployment
metadata:
  ## Name and labels of the Deployment
  labels:
    app: backend
  name: backend
spec:
  replicas: 1
  selector:
    matchLabels:
      app: backend
  template:
    metadata:
      labels:
        app: backend
    spec:
      containers:
      ## The container to launch
      - image: backend
        name: backend
        imagePullPolicy: Never
EOF
```


## Deploy Dockerwatch

Dockerwatch is deployed as previously:

```
> kubectl create service nodeport dockerwatch --tcp=8080:8080 --tcp=8443:8443
service/dockerwatch created
> ./create-certs $(minikube ip)
......
> kubectl create secret generic dockerwatch --from-file=ssl/
secret/dockerwatch created
> eval $(minikube docker-env)
> docker build -t dockerwatch .
```

with some small modifications to the config:

```
cat <<EOF | kubectl apply -f -
apiVersion: apps/v1
kind: Deployment
metadata:
  ## Name and labels of the Deployment
  labels:
    app: dockerwatch
  name: dockerwatch
spec:
  replicas: 10
  selector:
    matchLabels:
      app: dockerwatch
  template:
    metadata:
      labels:
        app: dockerwatch
    spec:
      containers:
      ## The container to launch
      - image: dockerwatch
        name: dockerwatch
        imagePullPolicy: Never
        ports:
        - containerPort: 8080
          protocol: TCP
        - containerPort: 8443
          protocol: TCP
        volumeMounts:
            - name: kube-keypair
              readOnly: true
              mountPath: /etc/ssl/certs
        env:
        - name: IP
          valueFrom:
            fieldRef:
              fieldPath: status.podIP
      volumes:
        - name: kube-keypair
          secret:
            secretName: dockerwatch
EOF
```

## Testing

We can then test the API using the same curl commands as the [simple demo](http://github.com/erlang/docker-erlang-example/tree/minikube-simple):

```
> curl -H "Content-Type: application/json" -X POST -d "" $(minikube service dockerwatch --url | head -1)/cnt
> curl -H "Content-Type: application/json" -X POST -d "{}" $(minikube service dockerwatch --url | head -1)/cnt
> curl --cacert ssl/dockerwatch-ca.pem -H "Accept: application/json" $(minikube service dockerwatch --url --https | tail -1)
["cnt"]
> curl --cacert ssl/dockerwatch-ca.pem -H "Accept: application/json" $(minikube service dockerwatch --url --https | tail -1)/cnt
{"cnt":2}
```
