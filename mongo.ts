Query Breakdown
Matching Product:
The query starts by filtering the Product collection to find the specific product by its productName.
Unwinding Sections:
The $unwind operator is used to break down the sections array into individual documents for easier processing.
Lookups:
The query performs lookups to join the Sections and Fields collections with the product based on their IDs.
Conditional Logic with addFields:
For App Provided: The query first checks for the provided app. If mDefault is provided in the input and is true, it directly checks for the mDefault value and skips the default logic. If mDefault is not provided, it checks the default and role logic.
For No App Provided: The fallback is to check for "app": "default" and apply the role logic if the role is present.
Projection:
Finally, the query projects the relevant fields, including the productName, sectionName, and isRequired status for sections and fields.



db.product.aggregate([
    {
        $match: { productName: "Product 1" }  // Match the specific product
    },
    {
        $unwind: "$sections"  // Unwind the sections array
    },
    {
        $lookup: {
            from: "section",
            localField: "sections",
            foreignField: "_id",
            as: "sectionDetails"
        }
    },
    { $unwind: "$sectionDetails" },
    {
        $lookup: {
            from: "fields",
            localField: "sectionDetails.fields",
            foreignField: "_id",
            as: "fieldDetails"
        }
    },
    {
        $addFields: {
            "sectionDetails.isVisible": {
                $cond: {
                    if: {
                        $and: [
                            { $eq: ["$sectionDetails.visibility.app", app] },  // Provided app check
                            {
                                $cond: {
                                    if: { $eq: [mDefault, true] },  // If mDefault is true
                                    then: { $eq: ["$sectionDetails.visibility.mDefault", true] },
                                    else: {  // If mDefault is not provided, fallback to default and role
                                        $cond: {
                                            if: { $eq: ["$sectionDetails.visibility.default", true] },
                                            then: {
                                                $or: [
                                                    { $in: [role, "$sectionDetails.visibility.role"] },
                                                    { $eq: ["$sectionDetails.visibility.default", true] }
                                                ]
                                            },
                                            else: false
                                        }
                                    }
                                }
                            }
                        ]
                    },
                    then: true,
                    else: {  // Fallback to default if no app provided
                        $cond: {
                            if: {
                                $and: [
                                    { $eq: ["$sectionDetails.visibility.app", "default"] },
                                    {
                                        $cond: {
                                            if: { $gt: [{ $size: "$sectionDetails.visibility.role" }, 0] },  // If role array is present and not empty
                                            then: { $in: [role, "$sectionDetails.visibility.role"] },
                                            else: { $eq: ["$sectionDetails.visibility.default", true] }
                                        }
                                    }
                                ]
                            },
                            then: true,
                            else: false
                        }
                    }
                }
            },
            "sectionDetails.isEditable": {
                $cond: {
                    if: {
                        $and: [
                            { $eq: ["$sectionDetails.isEditable.app", app] },  // Provided app check
                            {
                                $cond: {
                                    if: { $eq: [mDefault, true] },  // If mDefault is true
                                    then: { $eq: ["$sectionDetails.isEditable.mDefault", true] },
                                    else: {  // If mDefault is not provided, fallback to default and role
                                        $cond: {
                                            if: { $eq: ["$sectionDetails.isEditable.default", true] },
                                            then: {
                                                $or: [
                                                    { $in: [role, "$sectionDetails.isEditable.role"] },
                                                    { $eq: ["$sectionDetails.isEditable.default", true] }
                                                ]
                                            },
                                            else: false
                                        }
                                    }
                                }
                            }
                        ]
                    },
                    then: true,
                    else: {  // Fallback to default if no app provided
                        $cond: {
                            if: {
                                $and: [
                                    { $eq: ["$sectionDetails.isEditable.app", "default"] },
                                    {
                                        $cond: {
                                            if: { $gt: [{ $size: "$sectionDetails.isEditable.role" }, 0] },  // If role array is present and not empty
                                            then: { $in: [role, "$sectionDetails.isEditable.role"] },
                                            else: { $eq: ["$sectionDetails.isEditable.default", true] }
                                        }
                                    }
                                ]
                            },
                            then: true,
                            else: false
                        }
                    }
                }
            },
            "sectionDetails.isRequired": {
                $cond: {
                    if: {
                        $and: [
                            { $eq: ["$sectionDetails.isRequired.app", app] },  // Provided app check
                            {
                                $cond: {
                                    if: { $eq: [mDefault, true] },  // If mDefault is true
                                    then: { $eq: ["$sectionDetails.isRequired.mDefault", true] },
                                    else: {  // If mDefault is not provided, fallback to default and role
                                        $cond: {
                                            if: { $eq: ["$sectionDetails.isRequired.default", true] },
                                            then: {
                                                $or: [
                                                    { $in: [role, "$sectionDetails.isRequired.role"] },
                                                    { $eq: ["$sectionDetails.isRequired.default", true] }
                                                ]
                                            },
                                            else: false
                                        }
                                    }
                                }
                            }
                        ]
                    },
                    then: true,
                    else: {  // Fallback to default if no app provided
                        $cond: {
                            if: {
                                $and: [
                                    { $eq: ["$sectionDetails.isRequired.app", "default"] },
                                    {
                                        $cond: {
                                            if: { $gt: [{ $size: "$sectionDetails.isRequired.role" }, 0] },  // If role array is present and not empty
                                            then: { $in: [role, "$sectionDetails.isRequired.role"] },
                                            else: { $eq: ["$sectionDetails.isRequired.default", true] }
                                        }
                                    }
                                ]
                            },
                            then: true,
                            else: false
                        }
                    }
                }
            },
            "fieldDetails": {
                $map: {
                    input: "$fieldDetails",
                    as: "field",
                    in: {
                        fieldName: "$$field.fieldName",
                        isVisible: {
                            $cond: {
                                if: {
                                    $and: [
                                        { $eq: ["$$field.visibility.app", app] },  // Provided app check
                                        {
                                            $cond: {
                                                if: { $eq: [mDefault, true] },  // If mDefault is true
                                                then: { $eq: ["$$field.visibility.mDefault", true] },
                                                else: {
                                                    $cond: {
                                                        if: { $eq: ["$$field.visibility.default", true] },
                                                        then: {
                                                            $or: [
                                                                { $in: [role, "$$field.visibility.role"] },
                                                                { $eq: ["$$field.visibility.default", true] }
                                                            ]
                                                        },
                                                        else: false
                                                    }
                                                }
                                            }
                                        }
                                    ]
                                },
                                then: true,
                                else: {
                                    $cond: {
                                        if: {
                                            $and: [
                                                { $eq: ["$$field.visibility.app", "default"] },
                                                {
                                                    $cond: {
                                                        if: { $gt: [{ $size: "$$field.visibility.role" }, 0] },  // If role array is present and not empty
                                                        then: { $in: [role, "$$field.visibility.role"] },
                                                        else: { $eq: ["$$field.visibility.default", true] }
                                                    }
                                                }
                                            ]
                                        },
                                        then: true,
                                        else: false
                                    }
                                }
                            }
                        },
                        isEditable: {
                            $cond: {
                                if: {
                                    $and: [
                                        { $eq: ["$$field.isEditable.app", app] },  // Provided app check
                                        {
                                            $cond: {
                                                if: { $eq: [mDefault, true] },  // If mDefault is true
                                                then: { $eq: ["$$field.isEditable.mDefault", true] },
                                                else: {
                                                    $cond: {
                                                        if: { $eq: ["$$field.isEditable.default", true] },
                                                        then: {
                                                            $or: [
                                                                { $in: [role, "$$field.isEditable.role"] },
                                                                { $eq: ["$$field.isEditable.default", true] }
                                                            ]
                                                        },
                                                        else: false
                                                    }
                                                }
                                            }
                                        }
                                    ]
                                },
                                then: true,
                                else: {
                                    $cond: {
                                        if: {
                                            $and: [
                                                { $eq: ["$$field.isEditable.app", "default"] },
                                                {
                                                    $cond: {
                                                        if: { $gt: [{ $size: "$$field.isEditable.role" }, 0] },  // If role array is present and not empty
                                                        then: { $in: [role, "$$field.isEditable.role"] },
                                                        else: { $eq: ["$$field.isEditable.default", true] }
                                                    }
                                                }
                                            ]
                                        },
                                        then: true,
                                        else: false
                                    }
                                }
                            }
                        },
                        isRequired: {
                            $cond: {
                                if: {
                                    $and: [
                                        { $eq: ["$$field.isRequired.app", app] },  // Provided app check
                                        {
                                            $cond: {
                                                if: { $eq: [mDefault, true] },  // If mDefault is true
                                                then: { $eq: ["$$field.isRequired.mDefault", true] },
                                                else: {
                                                    $cond: {
                                                        if: { $eq: ["$$field.isRequired.default", true] },
                                                        then: {
                                                            $or: [
                                                                { $in: [role, "$$field.isRequired.role"] },
                                                                { $eq: ["$$field.isRequired.default", true] }
                                                            ]
                                                        },
                                                        else: false
                                                    }
                                                }
                                            }
                                        }
                                    ]
                                },
                                then: true,
                                else: {
                                    $cond: {
                                        if: {
                                            $and: [
                                                { $eq: ["$$field.isRequired.app", "default"] },
                                                {
                                                    $cond: {
                                                        if: { $gt: [{ $size: "$$field.isRequired.role" }, 0] },  // If role array is present and not empty
                                                        then: { $in: [role, "$$field.isRequired.role"] },
                                                        else: { $eq: ["$$field.isRequired.default", true] }
                                                    }
                                                }
                                            ]
                                        },
                                        then: true,
                                        else: false
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    },
    {
        $project: {
            productName: 1,
            "sectionDetails.sectionName": 1,
            "sectionDetails.isVisible": 1,
            "sectionDetails.isEditable": 1,
            "sectionDetails.isRequired": 1,
            "sectionDetails.fieldDetails.fieldName": 1,
            "sectionDetails.fieldDetails.isVisible": 1,
            "sectionDetails.fieldDetails.isEditable": 1,
            "sectionDetails.fieldDetails.isRequired": 1
        }
    }
]);



-----------------------------------------------------------

{
    "productName": "Product 1",
    "sectionDetails": [
        {
            "sectionName": "Section 1",
            "isVisible": true,
            "isEditable": true,
            "isRequired": true,
            "fieldDetails": [
                {
                    "fieldName": "Some Field Name",
                    "isVisible": true,
                    "isEditable": true,
                    "isRequired": false
                },
                {
                    "fieldName": "Another Field Name",
                    "isVisible": false,
                    "isEditable": false,
                    "isRequired": false
                }
            ]
        },
        {
            "sectionName": "Section 2",
            "isVisible": true,
            "isEditable": false,
            "isRequired": true,
            "fieldDetails": [
                {
                    "fieldName": "Third Field Name",
                    "isVisible": true,
                    "isEditable": false,
                    "isRequired": true
                }
            ]
        }
    ]
}

---------------------
Java code

import com.mongodb.client.AggregateIterable;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import org.bson.Document;

import java.util.Arrays;
import java.util.List;

public class MongoDBAggregationExample {

    public static void main(String[] args) {
        String uri = "your_mongodb_connection_string";

        try (MongoClient mongoClient = MongoClients.create(uri)) {

            MongoDatabase database = mongoClient.getDatabase("yourDatabaseName");
            MongoCollection<Document> collection = database.getCollection("product");

            // Example input
            String productName = "Product 1";
            String app = "app1";  // or set to null if app is not provided
            boolean mDefault = true;  // or false
            List<String> roles = Arrays.asList("admin", "nao");  // Example roles input

            // Aggregation pipeline
            List<Document> pipeline = Arrays.asList(
                new Document("$match", new Document("productName", productName)),
                new Document("$unwind", "$sections"),
                new Document("$lookup", new Document("from", "section")
                    .append("localField", "sections")
                    .append("foreignField", "_id")
                    .append("as", "sectionDetails")),
                new Document("$unwind", "$sectionDetails"),
                new Document("$lookup", new Document("from", "fields")
                    .append("localField", "sectionDetails.fields")
                    .append("foreignField", "_id")
                    .append("as", "fieldDetails")),
                new Document("$addFields", new Document("sectionDetails.isVisible",
                    new Document("$cond", Arrays.asList(
                        new Document("$and", Arrays.asList(
                            new Document("$eq", Arrays.asList("$sectionDetails.visibility.app", app)),
                            new Document("$cond", Arrays.asList(
                                new Document("$eq", Arrays.asList(mDefault, true)),
                                new Document("$eq", Arrays.asList("$sectionDetails.visibility.mDefault", true)),
                                new Document("$cond", Arrays.asList(
                                    new Document("$eq", Arrays.asList("$sectionDetails.visibility.default", true)),
                                    new Document("$or", Arrays.asList(
                                        new Document("$in", Arrays.asList(roles, "$sectionDetails.visibility.role")),
                                        new Document("$eq", Arrays.asList("$sectionDetails.visibility.default", true))
                                    )),
                                    false
                                ))
                            ))
                        )),
                        true,
                        new Document("$cond", Arrays.asList(
                            new Document("$and", Arrays.asList(
                                new Document("$eq", Arrays.asList("$sectionDetails.visibility.app", "default")),
                                new Document("$cond", Arrays.asList(
                                    new Document("$gt", Arrays.asList(new Document("$size", "$sectionDetails.visibility.role"), 0)),
                                    new Document("$in", Arrays.asList(roles, "$sectionDetails.visibility.role")),
                                    new Document("$eq", Arrays.asList("$sectionDetails.visibility.default", true))
                                ))
                            )),
                            true,
                            false
                        ))
                    ))
                )),
                new Document("$addFields", new Document("sectionDetails.isEditable",
                    new Document("$cond", Arrays.asList(
                        new Document("$and", Arrays.asList(
                            new Document("$eq", Arrays.asList("$sectionDetails.isEditable.app", app)),
                            new Document("$cond", Arrays.asList(
                                new Document("$eq", Arrays.asList(mDefault, true)),
                                new Document("$eq", Arrays.asList("$sectionDetails.isEditable.mDefault", true)),
                                new Document("$cond", Arrays.asList(
                                    new Document("$eq", Arrays.asList("$sectionDetails.isEditable.default", true)),
                                    new Document("$or", Arrays.asList(
                                        new Document("$in", Arrays.asList(roles, "$sectionDetails.isEditable.role")),
                                        new Document("$eq", Arrays.asList("$sectionDetails.isEditable.default", true))
                                    )),
                                    false
                                ))
                            ))
                        )),
                        true,
                        new Document("$cond", Arrays.asList(
                            new Document("$and", Arrays.asList(
                                new Document("$eq", Arrays.asList("$sectionDetails.isEditable.app", "default")),
                                new Document("$cond", Arrays.asList(
                                    new Document("$gt", Arrays.asList(new Document("$size", "$sectionDetails.isEditable.role"), 0)),
                                    new Document("$in", Arrays.asList(roles, "$sectionDetails.isEditable.role")),
                                    new Document("$eq", Arrays.asList("$sectionDetails.isEditable.default", true))
                                ))
                            )),
                            true,
                            false
                        ))
                    ))
                )),
                new Document("$addFields", new Document("sectionDetails.isRequired",
                    new Document("$cond", Arrays.asList(
                        new Document("$and", Arrays.asList(
                            new Document("$eq", Arrays.asList("$sectionDetails.isRequired.app", app)),
                            new Document("$cond", Arrays.asList(
                                new Document("$eq", Arrays.asList(mDefault, true)),
                                new Document("$eq", Arrays.asList("$sectionDetails.isRequired.mDefault", true)),
                                new Document("$cond", Arrays.asList(
                                    new Document("$eq", Arrays.asList("$sectionDetails.isRequired.default", true)),
                                    new Document("$or", Arrays.asList(
                                        new Document("$in", Arrays.asList(roles, "$sectionDetails.isRequired.role")),
                                        new Document("$eq", Arrays.asList("$sectionDetails.isRequired.default", true))
                                    )),
                                    false
                                ))
                            ))
                        )),
                        true,
                        new Document("$cond", Arrays.asList(
                            new Document("$and", Arrays.asList(
                                new Document("$eq", Arrays.asList("$sectionDetails.isRequired.app", "default")),
                                new Document("$cond", Arrays.asList(
                                    new Document("$gt", Arrays.asList(new Document("$size", "$sectionDetails.isRequired.role"), 0)),
                                    new Document("$in", Arrays.asList(roles, "$sectionDetails.isRequired.role")),
                                    new Document("$eq", Arrays.asList("$sectionDetails.isRequired.default", true))
                                ))
                            )),
                            true,
                            false
                        ))
                    ))
                )),
                new Document("$addFields", new Document("fieldDetails",
                    new Document("$map", new Document("input", "$fieldDetails")
                        .append("as", "field")
                        .append("in", new Document("fieldName", "$$field.fieldName")
                            .append("isVisible", new Document("$cond", Arrays.asList(
                                new Document("$and", Arrays.asList(
                                    new Document("$eq", Arrays.asList("$$field.visibility.app", app)),
                                    new Document("$cond", Arrays.asList(
                                        new Document("$eq", Arrays.asList(mDefault, true)),
                                        new Document("$eq", Arrays.asList("$$field.visibility.mDefault", true)),
                                        new Document("$cond", Arrays.asList(
                                            new Document("$eq", Arrays.asList("$$field.visibility.default", true)),
                                            new Document("$or", Arrays.asList(
                                                new Document("$in", Arrays.asList(roles, "$$field.visibility.role")),
                                                new Document("$eq", Arrays.asList("$$field.visibility.default", true))
                                            )),
                                            false
                                        ))
                                    ))
                                )),
                                true,
                                new Document("$cond", Arrays.asList(
                                    new Document("$and", Arrays.asList(
                                        new Document("$eq", Arrays.asList("$$field.visibility.app", "default")),
                                        new Document("$cond", Arrays.asList(
                                            new Document("$gt", Arrays.asList(new Document("$size", "$$field.visibility.role"), 0)),
                                            new Document("$in", Arrays.asList(roles, "$$field.visibility.role")),
                                            new Document("$eq", Arrays.asList("$$field.visibility.default", true))
                                        ))
                                    )),
                                    true,
                                    false
                                ))
                            )))
                            .append("isEditable", new Document("$cond", Arrays.asList(
                                new Document("$and", Arrays.asList(
                                    new Document("$eq", Arrays.asList("$$field.isEditable.app", app)),
                                    new Document("$cond", Arrays.asList(
                                        new Document("$eq", Arrays.asList(mDefault, true)),
                                        new Document("$eq", Arrays.asList("$$field.isEditable.mDefault", true)),
                                        new Document("$cond", Arrays.asList(
                                            new Document("$eq", Arrays.asList("$$field.isEditable.default", true)),
                                            new Document("$or", Arrays.asList(
                                                new Document("$in", Arrays.asList(roles, "$$field.isEditable.role")),
                                                new Document("$eq", Arrays.asList("$$field.isEditable.default", true))
                                            )),
                                            false
                                        ))
                                    ))
                                )),
                                true,
                                new Document("$cond", Arrays.asList(
                                    new Document("$and", Arrays.asList(
                                        new Document("$eq", Arrays.asList("$$field.isEditable.app", "default")),
                                        new Document("$cond", Arrays.asList(
                                            new Document("$gt", Arrays.asList(new Document("$size", "$$field.isEditable.role"), 0)),
                                            new Document("$in", Arrays.asList(roles, "$$field.isEditable.role")),
                                            new Document("$eq", Arrays.asList("$$field.isEditable.default", true))
                                        ))
                                    )),
                                    true,
                                    false
                                ))
                            )))
                            .append("isRequired", new Document("$cond", Arrays.asList(
                                new Document("$and", Arrays.asList(
                                    new Document("$eq", Arrays.asList("$$field.isRequired.app", app)),
                                    new Document("$cond", Arrays.asList(
                                        new Document("$eq", Arrays.asList(mDefault, true)),
                                        new Document("$eq", Arrays.asList("$$field.isRequired.mDefault", true)),
                                        new Document("$cond", Arrays.asList(
                                            new Document("$eq", Arrays.asList("$$field.isRequired.default", true)),
                                            new Document("$or", Arrays.asList(
                                                new Document("$in", Arrays.asList(roles, "$$field.isRequired.role")),
                                                new Document("$eq", Arrays.asList("$$field.isRequired.default", true))
                                            )),
                                            false
                                        ))
                                    ))
                                )),
                                true,
                                new Document("$cond", Arrays.asList(
                                    new Document("$and", Arrays.asList(
                                        new Document("$eq", Arrays.asList("$$field.isRequired.app", "default")),
                                        new Document("$cond", Arrays.asList(
                                            new Document("$gt", Arrays.asList(new Document("$size", "$$field.isRequired.role"), 0)),
                                            new Document("$in", Arrays.asList(roles, "$$field.isRequired.role")),
                                            new Document("$eq", Arrays.asList("$$field.isRequired.default", true))
                                        ))
                                    )),
                                    true,
                                    false
                                ))
                            )))
                        ))
                )),
                new Document("$project", new Document("productName", 1)
                    .append("sectionDetails.sectionName", 1)
                    .append("sectionDetails.isVisible", 1)
                    .append("sectionDetails.isEditable", 1)
                    .append("sectionDetails.isRequired", 1)
                    .append("sectionDetails.fieldDetails.fieldName", 1)
                    .append("sectionDetails.fieldDetails.isVisible", 1)
                    .append("sectionDetails.fieldDetails.isEditable", 1)
                    .append("sectionDetails.fieldDetails.isRequired", 1))
            );

            // Execute the aggregation query
            AggregateIterable<Document> result = collection.aggregate(pipeline);

            // Output the results
            for (Document doc : result) {
                System.out.println(doc.toJson());
            }
        }
    }
}
