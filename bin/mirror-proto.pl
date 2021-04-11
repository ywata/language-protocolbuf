#!/usr/bin/env perl
use strict;
#Files listed here is extracted with the command below on protobuf/src where protobuf
#is cloned with git clone https://github.com/protocolbuffers/protobuf
#find google -name "*.proto"  |grep -v internal |grep -v test|grep -v cpp |grep -v ruby

my @proto_files = ("google/protobuf/timestamp.proto",
		   "google/protobuf/field_mask.proto",
		   "google/protobuf/util/json_format.proto",
		   "google/protobuf/util/json_format_proto3.proto",
		   "google/protobuf/api.proto",
		   "google/protobuf/duration.proto",
		   "google/protobuf/struct.proto",
		   "google/protobuf/wrappers.proto",
		   "google/protobuf/source_context.proto",
		   "google/protobuf/any.proto",
		   "google/protobuf/type.proto",
		   "google/protobuf/empty.proto",
		   "google/protobuf/compiler/plugin.proto",
		   "google/protobuf/descriptor.proto");


if ($#ARGV < 0){
    &usage;
}

my $git_protobuf = $ARGV[0];
chomp(my $cwd = `pwd`);

if( ! -d "$git_protobuf/.git"){
    print STDERR "Specify git directory\n";    
    &usage;
}

my $git_hash = &getHash($git_protobuf);
&print_script($git_protobuf, $git_hash, @proto_files);

sub print_script{
    my($dir, $hash, @files) = @_;
    print <<"EOF";
mkdir -p proto
echo $hash > proto/.proto_git_hash
tar -C $dir/src -c @files | tar -C proto -x -
EOF
    foreach my $f (@files){
	print <<"EOF";
EOF
    }
}


sub getHash{
    my($dir) = @_;
    if(chdir($dir)){
	my $git_hash = `git log -1 --format=format:"%H"`;
	#    print "ERROR: $? $git_hash    $cwd\n";
	if($? > 0){
	    print STDERR "dir must be git repository\n";
	    &usage;
	}
	return $git_hash;
    }else{
	&usage;
    }
}



sub usage{
    print STDERR "usage: $0 dir\n";
    exit 1;
}
