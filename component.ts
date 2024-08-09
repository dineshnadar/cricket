db.product.aggregate([
    {
        $match: { _id: "product1" }  // Match the specific product
    },
    {
        $unwind: "$sections"  // Unwind the sections array
    },
    {
        $lookup: {
            from: "section",  // Name of the Section collection
            localField: "sections",
            foreignField: "_id",
            as: "sectionDetails"
        }
    },
    {
        $unwind: "$sectionDetails"  // Unwind the sectionDetails array
    },
    {
        $project: {
            productName: 1,
            "sectionDetails.sectionName": 1,
            "sectionDetails.visibility": {
                $filter: {
                    input: "$sectionDetails.visibility",
                    as: "visibility",
                    cond: { $eq: ["$$visibility.app", "app1"] }
                }
            },
            "sectionDetails.isEditable": {
                $filter: {
                    input: "$sectionDetails.isEditable",
                    as: "isEditable",
                    cond: { $eq: ["$$isEditable.app", "app1"] }
                }
            },
            "sectionDetails.isRequired": {
                $filter: {
                    input: "$sectionDetails.isRequired",
                    as: "isRequired",
                    cond: { $eq: ["$$isRequired.app", "app1"] }
                }
            },
            "sectionDetails.fields": 1
        }
    },
    {
        $lookup: {
            from: "fields",  // Name of the Fields collection
            localField: "sectionDetails.fields",
            foreignField: "_id",
            as: "fieldDetails"
        }
    },
    {
        $project: {
            productName: 1,
            "sectionDetails.sectionName": 1,
            "sectionDetails.visibility": { $arrayElemAt: ["$sectionDetails.visibility.isVisible", 0] },
            "sectionDetails.isEditable": { $arrayElemAt: ["$sectionDetails.isEditable.isEditable", 0] },
            "sectionDetails.isRequired": { $arrayElemAt: ["$sectionDetails.isRequired.isRequired", 0] },
            "fieldDetails": {
                $map: {
                    input: "$fieldDetails",
                    as: "field",
                    in: {
                        fieldName: "$$field.fieldName",
                        visibility: {
                            $arrayElemAt: [{
                                $filter: {
                                    input: "$$field.visibility",
                                    as: "vis",
                                    cond: { $eq: ["$$vis.app", "app1"] }
                                }
                            }, 0]
                        },
                        isEditable: {
                            $arrayElemAt: [{
                                $filter: {
                                    input: "$$field.isEditable",
                                    as: "edit",
                                    cond: { $eq: ["$$edit.app", "app1"] }
                                }
                            }, 0]
                        },
                        isRequired: {
                            $arrayElemAt: [{
                                $filter: {
                                    input: "$$field.isRequired",
                                    as: "req",
                                    cond: { $eq: ["$$req.app", "app1"] }
                                }
                            }, 0]
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
            "sectionDetails.visibility": 1,
            "sectionDetails.isEditable": 1,
            "sectionDetails.isRequired": 1,
            "fieldDetails.fieldName": 1,
            "fieldDetails.visibility.isVisible": 1,
            "fieldDetails.isEditable.isEditable": 1,
            "fieldDetails.isRequired.isRequired": 1
        }
    }
])
