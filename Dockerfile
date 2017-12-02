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
ADD https://github.com/erlang/otp/archive/OTP-19.2.tar.gz .
RUN tar zxf OTP-19.2.tar.gz
WORKDIR otp-OTP-19.2
RUN ./otp_build autoconf && \
    CFLAGS="-Os" ./configure --prefix=/buildroot/erlang/19.2 --without-termcap --disable-hipe && \
    make -j10

RUN mkdir /buildroot/ridhm_pubsub
COPY project /buildroot/ridhm_pubsub
WORKDIR /buildroot/ridhm_pubsub
RUN make
RUN make run
