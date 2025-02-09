#!/bin/sh

comparison_reference="generic/testCeylanMostBasic/testCeylanMostBasic.vcproj"

echo
echo "   Comparing project files to ensure homogeneity"
echo "   (reference is ${comparison_reference})"
echo



for f in */*/*.vcproj ; do 

	if [ ! "$f" = "${comparison_reference}" ] ; then
	
		echo "###### reference vs $f "######
		
		diff ${comparison_reference} $f  | grep -v 'Name='| grep -v ProjectGUID | grep -v RootNamespace | grep -v RelativePath | grep -v TargetFrameworkVersion
				
	fi
	
done | more

