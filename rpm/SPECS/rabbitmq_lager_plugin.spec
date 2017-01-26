

Summary:   RabbitMQ Topic ACL Authorization plugin
Name:      rabbitmq-topic-acl-plugin
Version:   1.0.0
Release:   0.el6
License:   AGPLv3
BuildRoot: %{_topdir}/BUILDROOT/
BuildArch: noarch
Requires:  rabbitmq-server = %{_rabbitmq_version}
Group:     Applications/Engineering
Vendor:    Telefonica I+D

%description
an authorization plugin for Rabbit MQ that will manage the access to RabbitMQ resources based on the Routing Keys used to publish and bind queues. The ultimate goal of this plugin is to be used along the RabbitMQ MQTT plugin in order to use RabbitMQ as an MQTT broker with ACL-based authorization.

########## WARNING FROM THE RABBITMQ WEBSITE ##################################
# The enabled plugins configuration is preserved between upgrades, 
# so there is no need to re-enable plugins after an upgrade, 
# but because the plugins directory changes between versions, 
# any third party plugins will need to be copied to the new directory. 
# It's very possible # that due to API changes you may need 
# to check for updates to third party plugins at this point.
###############################################################################


# System folders
%define _install_dir /usr/lib/rabbitmq/lib/rabbitmq_server-%{_rabbitmq_version}/plugins

# RPM Building folder
%define _build_root_project %{buildroot}/%{_install_dir}

# -------------------------------------------------------------------------------------------- #
# prep section, setup macro:
# -------------------------------------------------------------------------------------------- #
%prep
# read from SOURCES, write into BUILD

echo "[INFO] Preparing installation"
# Create rpm/BUILDROOT folder
rm -Rf $RPM_BUILD_ROOT && mkdir -p $RPM_BUILD_ROOT
[ -d %{_build_root_project} ] || mkdir -p %{_build_root_project}

# Copy all from src to rpm/BUILD
cp %{_srcdir}/lager-*.ez %{_build_root_project}
cp %{_srcdir}/goldrush-*.ez %{_build_root_project}

# -------------------------------------------------------------------------------------------- #
# pre-install section:
# -------------------------------------------------------------------------------------------- #
%pre


# -------------------------------------------------------------------------------------------- #
# post-install section:
# -------------------------------------------------------------------------------------------- #
%post


# -------------------------------------------------------------------------------------------- #
# pre-uninstall section:
# -------------------------------------------------------------------------------------------- #
%preun


# -------------------------------------------------------------------------------------------- #
# post-uninstall section:
# clean section:
# -------------------------------------------------------------------------------------------- #
%postun
%clean
rm -rf $RPM_BUILD_ROOT

# -------------------------------------------------------------------------------------------- #
# Files to add to the RPM
# -------------------------------------------------------------------------------------------- #
%files
%defattr(755,%{_project_user},%{_project_user},755)
# Copy all files under the _install_dir folder
%{_install_dir}

%changelog
* Mon Jan 23 2017 Rafael Gonzalez (rafael.gonzalezfuentetaja@telefonica.com) 1.0.0-0.0
- Initial lagger/goldrush plugin package (1.0.0)
