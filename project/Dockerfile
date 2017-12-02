FROM centos:centos7
RUN yum -y update && yum clean all
RUN yum install -y wget && yum clean all
RUN yum install -y epel-release && yum clean all
RUN yum update -y && yum upgrade -y
RUN yum install -y yum install gcc gcc-c++ glibc-devel make ncurses-devel openssl-devel autoconf java-1.8.0-openjdk-devel git && yum clean all
RUN yum install -y wxBase.x86_64 && yum clean all
RUN yum install -y http://packages.erlang-solutions.com/erlang-solutions-1.0-1.noarch.rpm && yum clean all
RUN yum install -y erlang && yum clean all

RUN mkdir /buildroot
WORKDIR /buildroot
RUN mkdir /ridhm_pubsub
COPY * ridhm_pubsub/
CMD  make run
