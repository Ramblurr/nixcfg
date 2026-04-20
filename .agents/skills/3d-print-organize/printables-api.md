# Printables API Notes

Keep this file short and current. Update it as new operations are discovered or existing ones drift.

Do not store real user IDs, collection IDs, tokens, cookies, or other personal identifiers here. Use placeholders only.

## Endpoint

- GraphQL endpoint: `https://api.printables.com/graphql/`
- There is no public schema.
- Learn operations from browser devtools requests and validate them against the live API.

## Auth Model

Public read queries can work without auth.

Authenticated reads and mutations use normal browser session state. In practice that means:

- `client-uid` header
- `graphql-client-version` header
- browser cookies, especially:
  - `csrftoken`
  - `cf_clearance`
  - `auth.access_token`
  - `auth.refresh_token`
  - related auth expiry cookies

Pragmatic rule: copy the browser request shape first, then simplify only after the operation works.

## Known Reads

### Print

Minimal public query:

```graphql
query Print($id: ID!) {
  print(id: $id) {
    id
    name
    slug
  }
}
```

Example variables:

```json
{"id":"<MODEL_ID>"}
```

### Model Files

Observed query shape:

```graphql
query ModelFiles($id: ID!) {
  model: print(id: $id) {
    id
    name
    slug
    filesType
    gcodes { id name folder note __typename }
    stls { id name folder note __typename }
    slas { id name folder note __typename }
    otherFiles { id name folder note __typename }
    downloadPacks { id name fileSize fileType __typename }
    __typename
  }
}
```

Observed response patterns:

- `filesType` can be values such as `GCODE`
- `downloadPacks` can include:
  - `MODEL_FILES`
  - `PRINT_FILES`
- `stls` may include both `.stl` and `.3mf` entries

This is the basis for downloading separate model-files and print-files zips.

### My Collections

Observed working query:

```graphql
query MyCollections($myId: ID!) {
  collections: userCollections(userId: $myId) {
    ...CollectionForSelection
    __typename
  }
}

fragment CollectionForSelection on CollectionType {
  id
  name
  private
  modelsCount: printsCount
  modified
  __typename
}
```

Example variables:

```json
{"myId":"<MY_USER_ID>"}
```

Schema drift note:

- A browser capture used `$myId: ID`
- The live API rejected that and required `$myId: ID!`

Observed behavior:

- Use the full browser-style authenticated request shape for private collections
- A reduced authenticated request may only expose public collections

## Known Mutations

### Like Model

Observed mutation:

```graphql
mutation LikeCreate($targetObjectId: ID!, $targetType: LikeTargetTypeEnum!) {
  likeCreate(targetObjectId: $targetObjectId, targetType: $targetType) {
    ok
    errors {
      ...Error
      __typename
    }
    count
    __typename
  }
}

fragment Error on ErrorType {
  field
  messages
  __typename
}
```

Example variables:

```json
{"targetObjectId":"<MODEL_ID>","targetType":"print"}
```

Observed response shape:

- `ok`
- `errors`
- `count`

Observed behavior:

- The mutation succeeded with a full browser-style request using `curl`
- The cookie set included `csrftoken`, `cf_clearance`, `auth.access_token`, `auth.refresh_token`, and related expiry cookies
- A reduced auth attempt returned a non-JSON `user_is_not_authenticated` response, so keep the mutation path close to the browser request shape

### Add Model To Collection

Verified mutation:

```graphql
mutation ModelAddToCollection($collectionId: ID!, $modelId: ID) {
  addPrintToCollection(collectionId: $collectionId, printId: $modelId) {
    ok
    errors {
      ...Error
      __typename
    }
    output {
      ...CollectionForSelection
      __typename
    }
    __typename
  }
}

fragment CollectionForSelection on CollectionType {
  id
  name
  private
  modelsCount: printsCount
  modified
  __typename
}

fragment Error on ErrorType {
  field
  messages
  __typename
}
```

Example variables:

```json
{"collectionId":"<COLLECTION_ID>","modelId":"<MODEL_ID>"}
```

Observed response shape:

- `ok`
- `errors`
- `output`

Observed behavior:

- The mutation succeeded with a full browser-style request using `curl`
- `output` came back as a collection list, not a single object
- The verified response included the target collection with updated metadata

## Download Link Mutation

Verified working mutation:

```graphql
mutation GetDownloadLink($printId: ID!, $ids: [ID!]!, $ft: DownloadFileTypeEnum!) {
  getDownloadLink(printId: $printId, source: model_detail, files: [{fileType: $ft, ids: $ids}]) {
    ok
    errors {
      field
      messages
      code
      __typename
    }
    output {
      link
      ttl
      __typename
    }
    __typename
  }
}
```

Example variables for a pack download:

```json
{"printId":"<MODEL_ID>","ids":["<PACK_ID>"],"ft":"pack"}
```

Observed behavior:

- `downloadPacks` from `ModelFiles` provides the pack IDs to use here
- the mutation returned signed URLs for both `MODEL_FILES` and `PRINT_FILES`
- the response includes `output.link` and `output.ttl`
- observed `ttl`: `86400`

## Workflow Notes

- Prefer Babashka plus ad hoc Clojure for exploration.
- Start from a copied browser request when working on authenticated operations.
- Validate operations against the live API before writing them into `SKILL.md`.
- Record schema drift here when an existing query or mutation stops working.
