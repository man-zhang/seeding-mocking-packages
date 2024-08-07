# Replication package for Seeding and Mocking in White-Box Fuzzing Enterprise RPC APIs: An Industrial Case Study 


In this repository, we provide necessary info in order to re-generate our analyses reported in the paper, such as tables and figures.

## Analysis and Raw Data
* [data fold](data) provides raw data produced during our experiments
    * [v2_0_coverage_json](data/v2_0_coverage_json) provides coverage reports archived by tests generated by EvoMaster. 
    * [zip files] are statistics outputted by EvoMaster.
  
Note that all confidential info (such as package path, class name) related to enterprise case studies must be removed.

* [analyze.R](analyze.R) is a script developed with R for performing analyses of results. The tables and figures can be automatically generated and stored into [generated_files](generated_files) by invoking the method `analyzeAll()`


## Implementation

Our implementation for supporting seeding and mocking has been integrated into EvoMaster v2.0.

To enable seeding and mocking, user can 

* define seeded tests with [SeededRPCTestDto](https://github.com/WebFuzzing/EvoMaster/blob/master/client-java/controller-api/src/main/java/org/evomaster/client/java/controller/api/dto/problem/rpc/SeededRPCTestDto.java);
* and implement following three method by extending `EmbeddedSutController` or `ExternalSutController`:
  * `List<SeededRPCTestDto> seedRPCTests()`: set seeded tests ;
  * `boolean customizeMockingRPCExternalService(List<MockRPCExternalServiceDto> externalServiceDtos, boolean enabled)`: employ in-house developed techniques for mocking external services;
  * `boolean customizeMockingDatabase(List<MockDatabaseDto> databaseDtos, boolean enabled)`: employ in-house developed techniques for mocking databases;

As EvoMaster is open-source, all info can be found online, eg, [evomaster webpage](https://www.evomaster.org), [evomaster v2.0](https://github.com/EMResearch/EvoMaster/releases/tag/v2.0.0).



