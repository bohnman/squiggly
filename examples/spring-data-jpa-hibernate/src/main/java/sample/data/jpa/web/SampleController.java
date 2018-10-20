/*
 * Copyright 2012-2016 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sample.data.jpa.web;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import sample.data.jpa.domain.City;
import sample.data.jpa.domain.Hotel;
import sample.data.jpa.domain.Review;
import sample.data.jpa.service.CityService;
import sample.data.jpa.service.HotelService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import sample.data.jpa.domain.City;
import sample.data.jpa.domain.Hotel;
import sample.data.jpa.domain.Review;
import sample.data.jpa.service.CityService;
import sample.data.jpa.service.HotelService;

@RestController
public class SampleController {

    @Autowired
    private CityService cityService;

    @Autowired
    private HotelService hotelService;

    @GetMapping("/")
    @ResponseBody
    @Transactional(readOnly = true)
    public String helloWorld() {
        return this.cityService.getCity("Bath", "UK").getName();
    }

    @GetMapping("/hotel")
    @ResponseBody
    @Transactional(readOnly = true)
    public Hotel hotel() {
        City city = this.cityService.getCity("Bath", "UK");
        String name = "The Bath Priory Hotel";
        Hotel hotel = this.hotelService.getHotel(city, name);
//		hotel.getReviews().size();
		return hotel;
	}


	@GetMapping("/reviews")
	@ResponseBody
	@Transactional(readOnly = true)
	public Page<Review> reviews() {
		return hotelService.getReviews(hotel(), PageRequest.of(0, 10));
	}

}
