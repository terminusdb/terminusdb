# The Ref schema

This is the Ref schema. It gives a specification for storage of references, branches and commits in our commit graph.

**Authored by:** Gavin Mendel-Gleason, Matthijs van Otterdijk

---

### Branch

<p class="tdb-f">A named branch object which points to the current head (or none if empty).</p>

**Class:** `Branch`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `name` | `xsd:string` | The name of the branch. |
| `head` | `Commit` | An optional commit object giving the present latest commit. |

---

### Commit

<p class="tdb-f">A commit object which has the various metadata associated with a commit</p>

**Class:** `Commit`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `instance` | `layer:Layer` | The (optional) current instance layer object. |
| `schema` | `layer:Layer` | The current schema layer object. |
| `author` | `xsd:string` | The author of the commit. |
| `message` | `xsd:string` | The message associated with the commit. |
| `identifier` | `xsd:string` | The identifier of the commit. |
| `timestamp` | `xsd:decimal` | The timestamp for the commit. |
| `parent` | `Commit` | The previous commit (if one exists). |

---

### InitialCommit

<p class="tdb-f">A special initial commit which is replaced when data exists.</p>

**Class:** `InitialCommit`

**Super class:** `Commit`

---

### InvalidCommit

<p class="tdb-f">The type of commits which have *failed* schema validation.</p>

**Class:** `InvalidCommit`

**Super class:** `Commit`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `instance` | `undefined` | The (optional) current instance layer object. |
| `schema` | `undefined` | The current schema layer object. |
| `author` | `undefined` | The author of the commit. |
| `message` | `undefined` | The message associated with the commit. |
| `identifier` | `undefined` | The identifier of the commit. |
| `timestamp` | `undefined` | The timestamp for the commit. |
| `parent` | `undefined` | The previous commit (if one exists). |

---

### Ref

<p class="tdb-f">A reference to a commit.</p>

**Class:** `Ref`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `identifier` | `xsd:string` | The identifier of the reference. |
| `commit` | `Commit` | The commit object pointed to by the reference. |

---

### Tag

<p class="tdb-f">A tag for a commit.</p>

**Class:** `Tag`

**Super class:** `Ref`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `identifier` | `undefined` | The identifier of the reference. |
| `commit` | `undefined` | The commit object pointed to by the reference. |

---

### ValidCommit

<p class="tdb-f">The type of commits which have passed schema validation.</p>

**Class:** `ValidCommit`

**Super class:** `Commit`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `instance` | `undefined` | The (optional) current instance layer object. |
| `schema` | `undefined` | The current schema layer object. |
| `author` | `undefined` | The author of the commit. |
| `message` | `undefined` | The message associated with the commit. |
| `identifier` | `undefined` | The identifier of the commit. |
| `timestamp` | `undefined` | The timestamp for the commit. |
| `parent` | `undefined` | The previous commit (if one exists). |

---

### layer:Layer

<p class="tdb-f">A layer object which has the identifier used in storage.</p>

**Class:** `layer:Layer`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `layer:identifier` | `xsd:string` | The layer identifier. |

---