
##amend photo_search to return the number of photos as well


photo_search_amended<-
  function(mindate_taken = NULL,
           maxdate_taken = NULL,
           mindate_uploaded = NULL,
           maxdate_uploaded = NULL,
           user_id = NULL,
           text = NULL,
           tags = NULL,
           tags_any = TRUE,
           bbox = NULL,
           woe_id = NULL,
           sf_layer = NULL,
           has_geo = TRUE) {

    pics <- NULL
    num_calls <- 0


    # create dfs so large searches can be subset dynamically
    date_df <- data.frame(mindate_taken = mindate_taken,
                          maxdate_taken = maxdate_taken)

    # this checks for the presence of a key, if no key it prompts the user to
    # create one, it then checks the validity of the key
    api_key <- create_and_check_key()

    # check that only one search location is given
    if ((!is.null(bbox) & !is.null(woe_id)) |
        (!is.null(sf_layer) & !is.null(woe_id)) |
        (!is.null(bbox) & !is.null(sf_layer))) {
      stop("Specify search location as only one of: woe_id, bbox or sf_layer.")
    }


    # change sf_layer to bbox
    if (!is.null(sf_layer)) {

      bbox <- create_bbox(sf_layer = sf_layer)

    }

    # check flickr location services work
    if (!is.null(woe_id)) {
      check_location(api_key = api_key)
    }

    #specify tag mode
    if (isTRUE(tags_any)){

      tags_any <- "any"

    } else {

      tags_any <- "all"

    }

    # start while loop - until all dates are looped through
    while (nrow(date_df) > 0) {

      # set search dates
      mindate_taken <- as.POSIXct(date_df[1, "mindate_taken"])
      maxdate_taken <- as.POSIXct(date_df[1, "maxdate_taken"])

      #flickr seems to search 8 hours in advance of
      mindate_taken <- mindate_taken - 28800

      mindate_unix <- as.numeric(mindate_taken)
      maxdate_unix <- as.numeric(maxdate_taken)

      if (mindate_taken > maxdate_taken){

        date_df <- date_df[-1, ]

      } else {

        base_url <- get_url(
          mindate_taken = mindate_unix,
          maxdate_taken = maxdate_unix,
          mindate_uploaded = mindate_uploaded,
          maxdate_uploaded = maxdate_uploaded,
          user_id = user_id,
          api_key = api_key,
          page = 1,
          text = text,
          tags = tags,
          tag_mode = tags_any,
          bbox = bbox,
          woe_id = woe_id,
          has_geo = has_geo
        )

        photo_xml <- search_url(base_url = base_url)

        #add to number of needed calls
        num_calls <- num_calls + 1

        find_errors(error_xml = photo_xml)

        if (!is.null(photo_xml)) {
          pages_data <- data.frame(
            xml2::xml_attrs(xml2::xml_children(photo_xml)))
          pages_data[] <- lapply(
            pages_data, FUN = function(x) as.integer(as.character(x)))
          total_pages <- pages_data["pages", ]
          total <- pages_data["total", ]



          if (total > 0 && total > 4000){

            for (i in 1:16){

              base_url <- get_url(
                mindate_taken = mindate_unix,
                maxdate_taken = maxdate_unix,
                mindate_uploaded = mindate_uploaded,
                maxdate_uploaded = maxdate_uploaded,
                user_id = user_id,
                api_key = api_key,
                page = i,
                text = text,
                tags = tags,
                tag_mode = tags_any,
                bbox = bbox,
                woe_id = woe_id,
                has_geo = has_geo
              )

                photo_xml <- search_url(base_url = base_url)

                #add to number of needed calls
                num_calls <- num_calls + 1

                if (!is.null(photo_xml)) {
                  photo_atts <- xml2::xml_find_all(
                    photo_xml, "//photo", ns = xml2::xml_ns(photo_xml))
                  tmp_df <- dplyr::bind_rows(lapply(
                    xml2::xml_attrs(photo_atts), function(x) data.frame(
                      as.list(x), stringsAsFactors = FALSE)))

                  #description is stored in seperate node
                  photo_atts2 <- xml2::xml_find_all(
                    photo_xml, "//description", ns = xml2::xml_ns(photo_xml))

                  descriptions <- NULL

                  for (i in 1:length(photo_atts2)){

                    tmp_df2 <- data.frame(description = paste(photo_atts2[i]))

                    descriptions <- rbind(descriptions, tmp_df2)

                    tmp_df2 <- NULL

                  }

                  descriptions$description <- gsub("<description>", "",
                                                   descriptions$description)

                  descriptions$description <- gsub("<description/>", "",
                                                   descriptions$description)

                  descriptions$description <- gsub("</description>", "",
                                                   descriptions$description)

                  #bind photos to descriptions
                  tmp_df <- cbind(tmp_df, descriptions)

                  pics <- dplyr::bind_rows(pics, tmp_df)

                  tmp_df <- NULL

                }

            }

            # create dataframe with minmaxdate_takens
            date_df <- rbind(
              date_df[-1, ],
              data.frame(mindate_taken = max(pics$datetaken),
                         maxdate_taken = maxdate_taken))

          } else if (total > 0 && total < 4000){

            for (i in 1:total_pages){

              base_url <- get_url(
                mindate_taken = mindate_unix,
                maxdate_taken = maxdate_unix,
                mindate_uploaded = mindate_uploaded,
                maxdate_uploaded = maxdate_uploaded,
                user_id = user_id,
                api_key = api_key,
                page = i,
                text = text,
                tags = tags,
                tag_mode = tags_any,
                bbox = bbox,
                woe_id = woe_id,
                has_geo = has_geo
              )

              photo_xml <- search_url(base_url = base_url)

              #add to number of needed calls
              num_calls <- num_calls + 1

              if (!is.null(photo_xml)) {
                photo_atts <- xml2::xml_find_all(
                  photo_xml, "//photo", ns = xml2::xml_ns(photo_xml))
                tmp_df <- dplyr::bind_rows(lapply(
                  xml2::xml_attrs(photo_atts), function(x) data.frame(
                    as.list(x), stringsAsFactors = FALSE)))

                #description is stored in seperate node
                photo_atts2 <- xml2::xml_find_all(
                  photo_xml, "//description", ns = xml2::xml_ns(photo_xml))

                descriptions <- NULL

                for (i in 1:length(photo_atts2)){

                  tmp_df2 <- data.frame(description = paste(photo_atts2[i]))

                  descriptions <- rbind(descriptions, tmp_df2)

                  tmp_df2 <- NULL

                }

                descriptions$description <- gsub("<description>", "",
                                                 descriptions$description)

                descriptions$description <- gsub("<description/>", "",
                                                 descriptions$description)

                descriptions$description <- gsub("</description>", "",
                                                 descriptions$description)

                #bind photos to descriptions
                tmp_df <- cbind(tmp_df, descriptions)

                pics <- dplyr::bind_rows(pics, tmp_df)

                tmp_df <- NULL
              }

            }

            # create dataframe with minmaxdate_takens
            date_df <- date_df[-1, ]

          } else {

            date_df <- date_df[-1, ]

          }


        } else {

          date_df <- date_df[-1, ]

        }

      }

    }

    if (is.null(pics)){

      stop("No photographs meeting criteria")

    }

    # is using sf_layer clip results to layer
    if (!is.null(sf_layer)) {
      with_geom <- sf::st_as_sf(pics,
                                coords = c("longitude", "latitude"),
                                crs = 4326)

      pics <- cbind(with_geom,
                    longitude = pics$longitude,
                    latitude = pics$latitude)

      sf_layer <- sf::st_transform(
        sf_layer, crs = "+proj=longlat +datum=WGS84 +no_defs")


      pics$within <- sf::st_intersects(pics, sf_layer)
      pics$within <- as.character(pics$within)
      pics <- dplyr::filter(pics, pics$within != "integer(0)")

    }

    pics <- parse_pic(pics = pics)

    pics <- dplyr::distinct(pics)




    # end
    return(list(pics,total))
  }
