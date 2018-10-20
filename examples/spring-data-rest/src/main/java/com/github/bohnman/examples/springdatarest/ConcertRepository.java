package com.github.bohnman.examples.springdatarest;

import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource(collectionResourceRel = "concerts", path = "concerts")
public interface ConcertRepository extends PagingAndSortingRepository<Concert, Long> {

    List<Concert> findByDescription(@Param("description") String description);

}
