// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import io.circe.literal._

class MutationSuite extends OdbSuite {

  queryTest(
    query = """
      mutation BulkEditConstraints($bulkEditConstraints: BulkEditConstraintSetInput!) {
        bulkEditConstraintSet(input: $bulkEditConstraints) {
          id
          constraintSet {
            skyBackground
          }
        }
      }
    """,
    expected = json"""
      {
        "bulkEditConstraintSet" : [
          {
            "id" : "o-3",
            "constraintSet" : {
              "skyBackground" : "GRAY"
            }
          },
          {
            "id" : "o-4",
            "constraintSet" : {
              "skyBackground" : "GRAY"
            }
          }
        ]
      }
    """,
    variables = Some(json"""
      {
        "bulkEditConstraints": {
          "select": {
            "observationIds": [ "o-3", "o-4" ]
          },
          "edit": {
            "skyBackground": "GRAY"
          }
        }
      }
    """)
  )

  queryTest(
    query = """
      mutation BulkEditScienceMode($bulkEditScienceMode: BulkEditScienceModeInput!) {
        bulkEditScienceMode(input: $bulkEditScienceMode) {
          id
          scienceMode {
            gmosSouthLongSlit {
              basic {
                grating
              }
              advanced {
                overrideGrating
              }
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "bulkEditScienceMode" : [
          {
            "id" : "o-3",
            "scienceMode": {
              "gmosSouthLongSlit": {
                "basic": {
                  "grating": "B600_G5323"
                },
                "advanced": {
                  "overrideGrating": "R600_G5324"
                }
              }
            }
          },
          {
            "id" : "o-4",
            "scienceMode": {
              "gmosSouthLongSlit": {
                "basic": {
                  "grating": "B600_G5323"
                },
                "advanced": {
                  "overrideGrating": "R600_G5324"
                }
              }
            }
          }
        ]
      }
    """,
    variables = Some(json"""
      {
        "bulkEditScienceMode": {
          "select": {
            "observationIds": [ "o-3", "o-4" ]
          },
          "edit": {
            "gmosSouthLongSlit": {
              "advanced": {
                "overrideGrating": "R600_G5324"
              }
            }
          }
        }
      }
    """)
  )

  // Attempts to edit the elevation range but it fails because the min range is
  // set to 0.  There should be only one error message even though the edit
  // would be to two observations.
  queryTestFailure(
    query =
      """
        mutation BulkEditConstraints($bulkEditConstraints: BulkEditConstraintSetInput!) {
          bulkEditConstraintSet(input: $bulkEditConstraints) {
            id
            constraintSet {
              skyBackground
              elevationRange {
                airMass {
                  min
                  max
                }
              }
            }
          }
        }
      """,
    errors = List(
      "'min' out of range: must be 1.0 <= min <= 3.0"
    ),
    variables = Some(json"""
      {
        "bulkEditConstraints": {
          "select": {
            "observationIds": [ "o-3", "o-4" ]
          },
          "edit": {
            "skyBackground": "GRAY",
            "elevationRange": {
              "airMass": {
                "min": 0.0,
                "max": 2.0
              }
            }
          }
        }
      }
      """)
  )


}