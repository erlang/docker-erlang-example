#!/bin/bash

set -x

export CHANGE_MINIKUBE_NONE_USER=true

# Download minikube.
MINIKUBE_VERSION=latest
curl -Lo minikube https://storage.googleapis.com/minikube/releases/$MINIKUBE_VERSION/minikube-linux-amd64 && chmod +x minikube && sudo mv minikube /usr/local/bin/
# Download kubectl, which is a requirement for using minikube.
KUBERNETES_VERSION=$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)
curl -Lo kubectl https://storage.googleapis.com/kubernetes-release/release/$KUBERNETES_VERSION/bin/linux/amd64/kubectl && chmod +x kubectl && sudo mv kubectl /usr/local/bin/
# Test that it works
kubectl -h
sudo minikube start -v 7 --logtostderr --vm-driver=none --kubernetes-version "$KUBERNETES_VERSION"
# Fix the kubectl context, as it's often stale.
minikube update-context
# Wait for Kubernetes to be up and ready.
JSONPATH='{range .items[*]}{@.metadata.name}:{range @.status.conditions[*]}{@.type}={@.status};{end}{end}'; until kubectl get nodes -o jsonpath="$JSONPATH" 2>&1 | grep -q "Ready=True"; do sleep 2; done

#script:
kubectl cluster-info
# kube-addon-manager is responsible for managing other kubernetes components, such as kube-dns, dashboard, storage-provisioner..
JSONPATH='{range .items[*]}{@.metadata.name}:{range @.status.conditions[*]}{@.type}={@.status};{end}{end}'; until kubectl -n kube-system get pods -lcomponent=kube-addon-manager -o jsonpath="$JSONPATH" 2>&1 | grep -q "Ready=True"; do sleep 10;echo "waiting for kube-addon-manager to be available"; kubectl get pods --all-namespaces; done
# Wait for kube-dns to be ready.
JSONPATH='{range .items[*]}{@.metadata.name}:{range @.status.conditions[*]}{@.type}={@.status};{end}{end}'; until kubectl -n kube-system get pods -lk8s-app=kube-dns -o jsonpath="$JSONPATH" 2>&1 | grep -q "Ready=True"; do sleep 10;echo "waiting for kube-dns to be available"; kubectl get pods --all-namespaces; done
kubectl create service nodeport dockerwatch --tcp=8080:8080 --tcp=8443:8443
kubectl get service
./create-certs $(minikube ip)
kubectl create secret generic dockerwatch --from-file=ssl/
kubectl get secret
docker build -t dockerwatch .
kubectl apply -f dockerwatch-deploy.yaml
# Wait for dockerwatch to be ready.
JSONPATH='{range .items[*]}{@.metadata.name}:{range @.status.conditions[*]}{@.type}={@.status};{end}{end}'; until kubectl -n default get pods -lapp=dockerwatch -o jsonpath="$JSONPATH" 2>&1 | grep -q "Ready=True"; do sleep 10;echo "waiting for dockerwatch to be available"; kubectl get pods --all-namespaces; done
HTTP=$(minikube service dockerwatch --url | head -1 | sed 's/* //g')
HTTPS=$(minikube service dockerwatch --url --https | tail -1 | sed 's/* //g')
curl -v -H 'Content-Type: application/json' -X POST -d '' $HTTP/cnt
curl -v -H 'Content-Type: application/json' -X POST -d '{}' $HTTP/cnt
curl -v --cacert ssl/dockerwatch-ca.pem -H 'Accept: application/json' $HTTPS/
curl -v --cacert ssl/dockerwatch-ca.pem -H 'Accept: application/json' $HTTPS/cnt
kubectl apply -f monitoring-namespace.yaml
kubectl apply -f prometheus-config.yaml
kubectl apply -f prometheus-deployment.yaml
kubectl apply -f prometheus-service.yaml
kubectl apply -f grafana-deployment.yaml
kubectl apply -f grafana-service.yaml
JSONPATH='{range .items[*]}{@.metadata.name}:{range @.status.conditions[*]}{@.type}={@.status};{end}{end}'; until kubectl -n monitoring get pods -lname=grafana -o jsonpath="$JSONPATH" 2>&1 | grep -q "Ready=True"; do sleep 10;echo "waiting for grafana to be available"; kubectl get pods --all-namespaces; done
JSONPATH='{range .items[*]}{@.metadata.name}:{range @.status.conditions[*]}{@.type}={@.status};{end}{end}'; until kubectl -n monitoring get pods -lname=prometheus -o jsonpath="$JSONPATH" 2>&1 | grep -q "Ready=True"; do sleep 10;echo "waiting for prometheus to be available"; kubectl get pods --all-namespaces; done

