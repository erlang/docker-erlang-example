# Using Minikube and Erlang

This is a quick demo of using minikube to run an Erlang node. The example we will
use is the [Docker Watch](http://github.com/erlang/docker-erlang-example/tree/master)
node.

This is only meant to be an example of how to get started. It is not the only,
nor neccesarily the best way to setup minikube with Erlang.

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

* Create a Service that will be used to access the dockerwatch API
* Create a Secret for our ssl keys
* Create a Deployment of dockerwatch that implements the Service

First however, make sure that the minikube cluster is started:

    > minikube start

and that you have cloned this repo and changed to the correct
directory:

    > git clone https://github.com/erlang/docker-erlang-example
    > cd docker-erlang-example/advanced_examples/minikube-simple

## Create a Service

The Service is what will be used to connect to the dockerwatch application
from outside the kubernetes cluster. It is not stricly neccesary to create the
Service before the deployment is done. However, it is considered good practice
to do so, as otherwise environment variables about the Service will not be
available in the Pods.

    > kubectl create service nodeport dockerwatch --tcp=8080:8080 --tcp=8443:8443
    service/dockerwatch created

Check that it was created:

    > kubectl get service
    NAME          TYPE        CLUSTER-IP      EXTERNAL-IP   PORT(S)                         AGE
    dockerwatch   NodePort    10.103.32.142   <none>        8080:31716/TCP,8443:30383/TCP   21m
    kubernetes    ClusterIP   10.96.0.1       <none>        443/TCP                         1h

We can see the external IP and port used through the minikube API:

    > minikube service dockerwatch --url
    http://192.168.99.101:31716
    http://192.168.99.101:30383

Take a note of the IP addresses you get as we will need them when creating the
ssl certifcates.

## Create a Secret

We then need to create a new Secret that will be used to store the ssl private key
and the certificate. The Secret will then be mounted into the running Pod just
as was done in the original example. We start by generating the CA and the
server certificate:

    > ./create-certs $(minikube ip)

Note that I put the IP address we got from `minikube service dockerwatch` above
as an argument. This is needed in order for SNI to work properly and in extension
to be able to connect to the service. The command will put a some files into
the ssl folder. We then use the `kubectl` command to create a Secret with those files.

    > kubectl create secret generic dockerwatch --from-file=ssl/
    secret/dockerwatch created

We can then see that the secret has been created using:

    > kubectl get secrets
    NAME          TYPE     DATA   AGE
    dockerwatch   Opaque   6      16s

## Deploy dockerwatch

Now that we have our Secret and Service configured we are ready to create the
dockerwatch Deployment. First we need to build the docker image just as in the
docker example. However to make things simple we will build the docker image
inside the kubernetes cluster. This is not really recommended, but it
simplifies things for these examples. In a realworld scenario you probably want
to setup your own docker registry.

So, to start with we need to setup our shell to contect the minikube docker
server instead of our local one. This is done through the `minikube` command:

    > eval $(minikube docker-env)

Then we can use `docker` as normal to build the image:

    > docker build -t dockerwatch .

Then the only thing that remains is to create the deployment.

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
  replicas: 1
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
        imagePullPolicy: Never ## Set to Never as we built the image in the cluster
        ports:
        - containerPort: 8080
          protocol: TCP
        - containerPort: 8443
          protocol: TCP
        volumeMounts:
            - name: kube-keypair
              readOnly: true
              mountPath: /etc/ssl/certs
      volumes:
        - name: kube-keypair
          secret:
            secretName: dockerwatch
EOF
```

This will create a Deployment called `dockerwatch` that run 1 replica
of the specified dockerwatch container. The container exposes ports 8080
and 8443 to the cluster and has the Secreat we created above mounted at
`/etc/ssl/certs`. Most of the configuration above should be fairly self
evident, refer to the kubernetes documentation for more details.

The Service will know that this is the Deployment to connect to by looking
at the metadata labels on the pods for a match to `app: dockerwatch`.

## Test the API

So now we have a kubernetes cluster running with an Erlang node in it, how do we
test that it works? `minikube` provides the answer through it's service API:

    > minikube service dockerwatch --url
    http://192.168.99.101:31716
    http://192.168.99.101:30383

The IP:Port pairs above are the external ports exposed to access our dockerwatch
service. So to work with the content we use the same REST API as in dockerwatch:

    > curl -H "Content-Type: application/json" -X POST -d "" http://192.168.99.101:31716/cnt
    > curl --cacert ssl/dockerwatch-ca.pem -H "Accept: application/json" https://192.168.99.101:30383
    ["cnt"]

## Further exploration

Minikube comes with the kubernetes dashboard, so we can easily look through a
web interface to look and change whatever we want. You access it by running:

    > minikube dashboard

This should open up a tab in your default browser where you can poke about. For
instance one thing to try is to change the number of replicas of the dockerwatch
ReplicaSet to and see what happens.
