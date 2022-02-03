import os,sys
import glob
import timeit
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib
from matplotlib import pyplot as plt
from collections import Counter
import math
import re
from itertools import islice
import difflib
import networkx as nx

# Checking the total time to run the code. START TIME here
start = timeit.default_timer()

# Directory that contains the data
datadir = 'PATENT CSV FORMAT DATA DIRECTORY'
os.chdir('{}'.format(datadir))

#### Compustat and Environmental Variables ####
## Extract CompuStat variables for organizations. Total Revenue, Total Assets, Number of Employees, R&D, SIC industry codes
## Return on Assets and Cash equivalent. Store them in Compustat dictionary using Gvkey of the organization.
compustat = {}
inputfile = open(datadir+'/compustat1970_2018.csv','r')
for line in inputfile.readlines():
	if not line.startswith("gvkey"):
		temp = line.strip('\n').split('\t')
		if temp[0] != "" and temp[2] != "" and float(temp[2]) >= 1975 and float(temp[2]) <= 2016:
			if temp[0].startswith("00"):
				gvkey = str(float(temp[0][2:]))
			elif temp[0].startswith("0"):
				gvkey = str(float(temp[0][1:]))
			else:
				gvkey = str(float(temp[0]))
			yr = temp[2]
			if temp[94] != '':
				totalAssets = temp[94]
			else:
				totalAssets = 0
			if temp[671] != '':
				totalRevenue = temp[671]
			else:
				totalRevenue = 0
			if temp[278] != '':
				employees = temp[278]
			else:
				employees = 0
			if temp[905] != '':
				rAndD = temp[905]
			else:
				rAndD = 0
			if temp[138] != '':
				cashAndEq = temp[138]
			else:
				cashAndEq = 0
			if temp[972] != '':
				sic = temp[972]
			else:
				sic = 0
			if temp[520] != '' and temp[94] != '':
				if float(temp[520]) != 0 and float(temp[94]) != 0:
					roa = float(temp[520])/float(temp[94])
				else:
					roa = 0
			else:
				roa = 0
			if gvkey in compustat:
				compustat[gvkey].append('{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}'.format(yr,totalAssets,totalRevenue,employees,rAndD,cashAndEq,sic,roa))
			else:
				compustat[gvkey] = ['{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}'.format(yr,totalAssets,totalRevenue,employees,rAndD,cashAndEq,sic,roa)]

inputfile.close()

## Append the yearly data in the order from 1975 - 2016 of an organization as a different value to the Gvkey of the 
## organization.
years = list(range(1975,2017,1))
compustatFull = {}
for key in compustat:
	sic = compustat[key][0].split('\t')[6]
	for yr in years:
		if any(re.search('^{}'.format(yr),s) for s in compustat[key]):
			indices = [i for i, s in enumerate(compustat[key]) if re.search('^{}'.format(yr),s) is not None]
			if key in compustatFull:
				compustatFull[key].append(compustat[key][indices[0]])
			else:
				compustatFull[key] = [compustat[key][indices[0]]]
		else:
			if key in compustatFull:
				compustatFull[key].append('{}\t0\t0\t0\t0\t0\t{}\t0'.format(str(float(yr)),sic))
			else:
				compustatFull[key] = ['{}\t0\t0\t0\t0\t0\t{}\t0'.format(str(float(yr)),sic)]	

print(next(iter(compustatFull.items())))

## Extract Environmental variables Dynamism and Munificence for each industry using SIC codes.
envComplex1 = {}
inputfile = open(datadir+'/env_measures.csv','r')
for line in inputfile.readlines():
	if not line.startswith("sic"):
		temp = line.strip('\n').split('\t')
		if temp[0] != "" and temp[1] != "" and float(temp[1]) >= 1975 and float(temp[1]) <= 2016:
			key = temp[0]+'-'+temp[1]
			envComplex1[key] = ['{}\t{}'.format(temp[7],temp[6])]

inputfile.close()
print(next(iter(envComplex1.items())))

## Extract Environmental variable Complexity for each industry using SIC codes.
envComplex2 = {}
inputfile = open(datadir+'/complexity_env.csv','r')
for line in inputfile.readlines():
	if not line.startswith("gvkey"):
		temp = line.strip('\n').split('\t')
		if temp[3] != "" and temp[1] != "" and float(temp[1]) >= 1975 and float(temp[1]) <= 2016:
			key = temp[3]+'-'+temp[1]
			envComplex2[key] = ['{}'.format(temp[4])]

inputfile.close()
print(next(iter(envComplex2.items())))

# Update the Year categorized Compustat dictionary with Environmental variables using SIC codes
for key in compustatFull:
	for i, val in enumerate(compustatFull[key]):
		temp = val.strip('\n').split('\t')
		sickey = temp[6]+'-'+temp[0]
		if sickey in envComplex1 and sickey in envComplex2:
			compustatFull[key][i] = val + '\t{}\t{}'.format(envComplex1[sickey][0],envComplex2[sickey][0])
		elif sickey in envComplex1 and sickey not in envComplex2:
			compustatFull[key][i] = val + '\t{}\t0'.format(envComplex1[sickey][0])
		elif sickey not in envComplex1 and sickey in envComplex2:
			compustatFull[key][i] = val + '\t0\t0\t{}'.format(envComplex2[sickey][0])
		else:
			compustatFull[key][i] = val + '\t0\t0\t0'

print(next(iter(compustatFull.items())))

# Print out the organization Compustat and Industry environment information into a CSV file
f= open(datadir+'/organizationPatentByYearCompustatEnvCombined.csv',"w")
f.write('Gvkey\tYear\tTotalAssets\tTotalRevenue\tEmployees\tR&D\tCashAndEquivalents\tSIC\tROA\tEnvironmentalDynamisim\tEnvironmentalMunifience\tEnvironmentalComplexity\n')
for key in compustatFull:
	for val in compustatFull[key]:
		f.write('{}\t{}\n'.format(key,val))

f.close()
compustat.clear()
envComplex1.clear()
envComplex2.clear()
compustatFull.clear()

### Patent --- Year dictionary
patentYear = {}
inputfile = open(datadir+'/org_patent_all_gvkeys_inventors.csv','r')
for line in inputfile.readlines():
	if not line.startswith("patentno"):
		temp = line.strip('\n').split('\t')
		if temp[0] != "" and temp[1] != "" and temp[3] != "":
			if temp[0] not in patentYear:
				patentYear[temp[0]] = temp[1]
			elif temp[0] in patentYear and patentYear[temp[0]] == temp[1]:
				patentYear[temp[0]] = temp[1]
			else:
				test = 1

inputfile.close()
print(next(iter(patentYear.items())))

#### Assignee Information
assignee = {}
inputfile = open(datadir+'/assignee_raw.csv','r')
for line in inputfile.readlines():
	if not line.startswith("id"):
		temp = line.strip().split('\t')
		assignee[temp[1]] = temp[2]

inputfile.close()
print(next(iter(assignee.items())))

## Create Patent and its assignee into dictionary using organization Gvkey
gvkeyAssignee = {}
inputfile = open(datadir+'/org_patent_all_gvkeys_inventors.csv','r')
for line in inputfile.readlines():
	if not line.startswith("patentno"):
		temp = line.strip('\n').split('\t')
		if temp[0] != "" and temp[1] != "" and temp[3] != "" and temp[0] in assignee:
			if temp[3] in gvkeyAssignee and assignee[temp[0]] not in gvkeyAssignee[temp[3]]:
				gvkeyAssignee[temp[3]].append(assignee[temp[0]])
			else:
				gvkeyAssignee[temp[3]] = [assignee[temp[0]]]

inputfile.close()

# Extract organization basic science related variables 
orgRaw = {}
count = 0
inputfile = open(datadir+'/org_patent_all_gvkeys_inventors.csv','r')
for line in inputfile.readlines():
	if not line.startswith("patentno"):
		temp = line.strip('\n').split('\t')
		if temp[0] != "" and temp[1] != "" and temp[3] != "":
			if temp[0] in assignee:
				companyName = assignee[temp[0]]
				if re.match('^(.*?)Co\.(.*?)Co\.(.*?)$',assignee[temp[0]],re.IGNORECASE) is not None:
					jointProject = "ForProfit"
				elif re.match('^(.*?)Ltd\.(.*?)Ltd\.(.*?)$',assignee[temp[0]],re.IGNORECASE) is not None:
					jointProject = "ForProfit"
				elif re.match('^(.*?)(?!University|Research)(.*?)Inc\.(.*?)(?!University|Research)Inc\.(.*)$',assignee[temp[0]],re.IGNORECASE) is not None:
					jointProject = "ForProfit"
				elif re.match('^(.*?)University(.*?)$',assignee[temp[0]],re.IGNORECASE) is not None:
					jointProject = "BasicScience"
				elif re.match('^(.*?)Government(.*?)$',assignee[temp[0]],re.IGNORECASE) is not None:
					jointProject = "BasicScience"
				elif re.match('^(.*?)nonprofit(.*?)$',assignee[temp[0]],re.IGNORECASE) is not None:
					jointProject = "BasicScience"
				else:
					jointProject = "0"
			else:
				companyName = "0"
				jointProject = "0"
			if temp[3] in orgRaw:
				if temp[5] == "":
					orgRaw[temp[3]].append('{}\t{}\t{}\t{}\t{}\t{}\t{}'.format(temp[1],temp[0],1,jointProject,temp[13],temp[44],companyName))
				else:
					orgRaw[temp[3]].append('{}\t{}\t{}\t{}\t{}\t{}\t{}'.format(temp[1],temp[0],temp[5],jointProject,temp[13],temp[44],companyName))
			else:
				if temp[5] == "":
					orgRaw[temp[3]] = ['{}\t{}\t{}\t{}\t{}\t{}\t{}'.format(temp[1],temp[0],1,jointProject,temp[13],temp[44],companyName)]
				else:
					orgRaw[temp[3]] = ['{}\t{}\t{}\t{}\t{}\t{}\t{}'.format(temp[1],temp[0],temp[5],jointProject,temp[13],temp[44],companyName)]
				count += 1
		elif temp[0] != "" and temp[1] != "" and temp[3] == "":
			if temp[0] in assignee:
				companyName = assignee[temp[0]]
				for key, value in gvkeyAssignee.items():
					if assignee[temp[0]] in value:
						gvkey = key
						break
				if re.match('^(.*?)Co\.(.*?)Co\.(.*?)$',assignee[temp[0]],re.IGNORECASE) is not None:
					jointProject = "ForProfit"
				elif re.match('^(.*?)Ltd\.(.*?)Ltd\.(.*?)$',assignee[temp[0]],re.IGNORECASE) is not None:
					jointProject = "ForProfit"
				elif re.match('^(.*?)(?!University|Research)(.*?)Inc\.(.*?)(?!University|Research)Inc\.(.*)$',assignee[temp[0]],re.IGNORECASE) is not None:
					jointProject = "ForProfit"
				elif re.match('^(.*?)University(.*?)$',assignee[temp[0]],re.IGNORECASE) is not None:
					jointProject = "BasicScience"
				elif re.match('^(.*?)Government(.*?)$',assignee[temp[0]],re.IGNORECASE) is not None:
					jointProject = "BasicScience"
				elif re.match('^(.*?)nonprofit(.*?)$',assignee[temp[0]],re.IGNORECASE) is not None:
					jointProject = "BasicScience"
				else:
					jointProject = "0"
				if gvkey in orgRaw:
					if temp[5] == "":
						orgRaw[gvkey].append('{}\t{}\t{}\t{}\t{}\t{}\t{}'.format(temp[1],temp[0],1,jointProject,temp[13],temp[44],companyName))
					else:
						orgRaw[gvkey].append('{}\t{}\t{}\t{}\t{}\t{}\t{}'.format(temp[1],temp[0],temp[5],jointProject,temp[13],temp[44],companyName))
				else:
					if temp[5] == "":
						orgRaw[gvkey] = ['{}\t{}\t{}\t{}\t{}\t{}\t{}'.format(temp[1],temp[0],1,jointProject,temp[13],temp[44],companyName)]
					else:
						orgRaw[gvkey] = ['{}\t{}\t{}\t{}\t{}\t{}\t{}'.format(temp[1],temp[0],temp[5],jointProject,temp[13],temp[44],companyName)]
				count += 1
			
inputfile.close()
assignee.clear()
gvkeyAssignee.clear()

### Calculate Patent Complexity which requires recombination rate
subGroupMap = {};
patentSubGroupMap = {};
inputfile = open(datadir+'/cpc.csv','r')
for line in inputfile.readlines():
	if not line.startswith("id"):
		temp = line.strip().split('\t')
		if temp[3] in subGroupMap:
			subGroupMap[temp[3]].append(temp[1])
		else:
			subGroupMap[temp[3]]=[temp[1]]
		if temp[1] in patentSubGroupMap:
			patentSubGroupMap[temp[1]].append(temp[3])
		else:
			patentSubGroupMap[temp[1]] = [temp[3]]
		if temp[3][0:4] in subClassMap:
			subClassMap[temp[3][0:4]].append(temp[1])
		else:
			subClassMap[temp[3][0:4]] = [temp[1]]
		if temp[1] in patentSubClassMap:
			patentSubClassMap[temp[1]].append(temp[3][0:4])
		else:
			patentSubClassMap[temp[1]] = [temp[3][0:4]]
	else:
		print('No recombination exists or missing!')

inputfile.close()

# SubGroup Recombination 
subGroupRecomb = {}
for key in subGroupMap.keys():
	flatList = [ item for elem in [patentSubGroupMap[k] for k in subGroupMap[key]] for item in elem]
	subGroupRecomb[key] = len(Counter(list(filter(lambda a: a != key, flatList))).keys())/len(subGroupMap[key])

patentSubGroupComplex = {}
for key in patentSubGroupMap.keys():
	patentSubGroupComplex[key] = (len(patentSubGroupMap[key])/(sum([subGroupRecomb[k] for k in patentSubGroupMap[key]])+1))/len(subGroupMap.keys())

subGroupRecomb.clear()

## Patent Impact
# Create Patent(p) --- patent(p) citation dictionary
p2pCites = {}
inputfile = open(datadir+'/citation.csv','r')
for line in inputfile.readlines():
	if not line.startswith("patentno"):
		temp = line.strip().split('\t')
		if temp[1] in p2pCites:
			p2pCites[temp[1]] += 1
		else:
			p2pCites[temp[1]] = 1

inputfile.close()
print(next(iter(p2pCites.items())))

# Patnet(p) --- patent(p) Inverse(Inv) citation dictionary
p2pCitesInv = {}
inputfile = open(datadir+'/citation.csv','r')
for line in inputfile.readlines():
	if not line.startswith("patentno"):
		temp = line.strip().split('\t')
		if temp[0] in p2pCitesInv:
			p2pCitesInv[temp[0]].append(temp[1])
		else:
			p2pCitesInv[temp[0]] = [temp[1]]

inputfile.close()
print(next(iter(p2pCitesInv.items())))

# Create Patent(p) --- non-patent(n) literature citation dictionary
p2nCites = {}
inputfile = open(datadir+'/pcs_mag_doi_pmid.tsv','r')
for line in inputfile.readlines():
	if not line.startswith("reftype"):
		temp = line.strip().split('\t')
		temp[3] = temp[3][4:]
		if int(temp[4]) == 1 and int(temp[1]) >= 3:
			if temp[3] in p2nCites:
				p2nCites[temp[3]] += 1
			else:
				p2nCites[temp[3]] = 1

inputfile.close()

## Patent Novelty - Normalized recombination rate
patentNovelty = []
for key in patentSubGroupMap:
	if len(patentSubGroupMap[key]) > 1:
		for i in range(0,len(patentSubGroupMap[key]),1):
			for j in range(i,len(patentSubGroupMap[key]),1):
				if i !=j and i < j:
					patentNovelty.append(patentSubGroupMap[key][i]+'-'+patentSubGroupMap[key][j])

patentSubGroupMap.clear()
subGroupPairs = dict(Counter(patentNovelty))
del patentNovelty
freq = 0
for value in subGroupPairs.values():
	freq += value

avg = freq/len(subGroupPairs.keys())
sd = 0
freq = 0
for value in subGroupPairs.values():
	freq += (value-avg) * (value-avg)

# Normalize the recombination rate
sd = math.sqrt(freq/len(subGroupPairs.keys()))
for key in subGroupPairs:
	subGroupPairs[key] = (subGroupPairs[key] - avg)/sd

patentNovelty = {}
for key in patentSubGroupMap:
	if len(patentSubGroupMap[key]) > 1:
		pairs = []
		sum = 0
		for i in range(0,len(patentSubGroupMap[key]),1):
			for j in range(i,len(patentSubGroupMap[key]),1):
				if i !=j and i < j:
					pairs.append(patentSubGroupMap[key][i]+'-'+patentSubGroupMap[key][j])
		for pair in pairs:
			sum += subGroupPairs[pair]
		patentNovelty[key] = sum
	else:
		patentNovelty[key] = 0

subGroupPairs.clear()

## Extract patent metrics for each Organization from years 1975 - 2016.
year = list(range(1975,2017,1))
orgPatentByYear = {}
for key in orgRaw:
	for yr in year:
		patentNumber = 0
		avgImpact = 0
		aggregateImpact = 0
		avgComplexity = 0
		aggregateComplexity = 0
		avgNovelty = 0
		aggregateNovelty = 0
		avgCompositeScore = 0
		aggregateCompositeScore = 0
		avgNpCites = 0
		aggregateNpCites = 0
		basicScience = 0
		forProfit = 0
		citesPastWork = 0
		citesBasicSicence = 0
		inventorCount = 0
		uniqueInventorCount = []
		avgInventor = 0
		patentName = []
		for val in orgRaw[key]:
			temp = val.strip('\n').split('\t')
			if float(temp[0]) == yr and temp[1] not in patentName:
				patentName.append(temp[1])
				patentNumber += 1
				if temp[1] in p2pCites:
					impact = p2pCites[temp[1]]
					aggregateImpact += p2pCites[temp[1]]
				else:
					impact = 0
					aggregateImpact += 0
				if temp[1] in patentSubGroupComplex:
					com = patentSubGroupComplex[temp[1]]
					aggregateComplexity += patentSubGroupComplex[temp[1]]
				else:
					com = 0
					aggregateComplexity += 0
				if temp[1] in patentNovelty:
					nov = patentNovelty[temp[1]]
					aggregateNovelty += patentNovelty[temp[1]]
				else:
					nov = 0
					aggregateNovelty += 0
				compositeScore = (1/3)*impact+(1/3)*com+(1/3)*nov
				aggregateCompositeScore += compositeScore
				inventorCount += float(temp[2])
				if temp[4] not in uniqueInventorCount and temp[4] != "":
					uniqueInventorCount.append(temp[4])
				if temp[1] in p2pCitesInv:
					for patent in p2pCitesInv[temp[1]]:
						if patent in patentYear:
							if float(patentYear[patent]) <= yr:
								citesPastWork += 1
							else:
								citesPastWork += 0
				else:
					citesPastWork += 0
				if temp[1] in p2nCites:
					aggregateNpCites += p2nCites[temp[1]]
				else:
					aggregateNpCites += 0
				if temp[3] == "BasicScience":
					basicScience += 1
				elif temp[3] == "ForProfit":
					forProfit += 1
				else:
					basicScience += 0
					forProfit += 0
		if patentNumber != 0:
			avgImpact = aggregateImpact / patentNumber
			avgComplexity = aggregateComplexity / patentNumber
			avgNovelty = aggregateNovelty / patentNumber
			avgCompositeScore = aggregateCompositeScore / patentNumber
			avgNpCites = aggregateNpCites / patentNumber
			avgInventor = inventorCount / patentNumber
		else:
			avgImpact = 0
			avgComplexity = 0
			avgNovelty = 0
			avgCompositeScore = 0
			avgNpCites = 0
			avgInventor = 0
		if key in orgPatentByYear:
			orgPatentByYear[key].append('{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n'.format(key,yr,patentNumber,avgImpact,aggregateImpact,avgComplexity,aggregateComplexity,avgNovelty,aggregateNovelty,avgCompositeScore,avgNpCites,aggregateNpCites,basicScience,forProfit,citesPastWork,citesBasicSicence,len(uniqueInventorCount)))
		else:
			orgPatentByYear[key] = ['{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n'.format(key,yr,patentNumber,avgImpact,aggregateImpact,avgComplexity,aggregateComplexity,avgNovelty,aggregateNovelty,avgCompositeScore,avgNpCites,aggregateNpCites,basicScience,forProfit,citesPastWork,citesBasicSicence,len(uniqueInventorCount))]

f= open(datadir+'/organizationPatentByYear3.csv',"w")
f.write('Gvkey\tYear\tNumberOfPatents\tAverageImpact\tAggregateImpact\tAverageComplexity\tAggregateComplexity\tAverageNovelty\tAggregateNovelty\tAverageComposite\tAverageNonPatentCites\tAggregateNonPatentCites\tJointProject(BasicScience)\tJointProject(ForProfit)\tCitesOfPastWork\tCitesOfBasicScience\tUniqueInventorCount\n')
for key in orgPatentByYear:
	for val in orgPatentByYear[key]:
		f.write('{}'.format(val))
		
f.close()
orgPatentByYear.clear()
orgRaw.clear()
p2pCites.clear()
p2nCites.clear()
p2pCitesInv.clear()
patentNovelty.clear()
patentSubGroupComplex.clear()

## Combine patent measures and other oreganizational measures
patentMeasures = {}
inputfile = open(datadir+'/organizationPatentByYear3.csv','r')
for line in inputfile.readlines():
	if not line.startswith("Gvkey"):
		temp = line.strip('\n').split('\t')
		key = temp[0]+'-'+str(float(temp[1]))
		patentMeasures[key] = '\t'.join(temp[2:])

combinedMeasures = {}
inputfile = open(datadir+'/organizationPatentByYearCompustatEnvCombined.csv','r')
for line in inputfile.readlines():
	if not line.startswith("Gvkey"):
		temp = line.strip('\n').split('\t')
		key = temp[0]+'-'+temp[1]
		combinedMeasures[key] = '\t'.join(temp[2:])

f= open(datadir+'/organizationPatentByYearFinal.csv',"w")
f.write('Gvkey\tYear\tNumberOfPatents\tAverageImpact\tAggregateImpact\tAverageComplexity\tAggregateComplexity\tAverageNovelty\tAggregateNovelty\tAverageComposite\tAverageNonPatentCites\tAggregateNonPatentCites\tJointProject(BasicScience)\tJointProject(ForProfit)\tCitesOfPastWork\tCitesOfBasicScience\tUniqueInventorCount\tTotalAssets\tTotalRevenue\tEmployees\tR&D\tCashAndEquivalents\tSIC\tROA\tEnvironmentalDynamisim\tEnvironmentalMunifience\tEnvironmentalComplexity\n')
for key in patentMeasures:
	gvkey = key.split('-')[0]
	yr = key.split('-')[1]
	if key in combinedMeasures:
		f.write('{}\t{}\t{}\t{}\n'.format(gvkey,yr,patentMeasures[key],combinedMeasures[key]))
	else:
		f.write('{}\t{}\t{}\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\n'.format(gvkey,yr,patentMeasures[key]))

f.close()
patentMeasures.clear()
combinedMeasures.clear()

