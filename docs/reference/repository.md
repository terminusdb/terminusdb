# Repo schema

This is the Repo schema. It is a specification for repository metadata management.

**Authored by:** Gavin Mendel-Gleason, Matthijs van Otterdijk

---

### Local

<p class="tdb-f">A local repository.</p>

**Class:** `Local`

**Super class:** `Repository`

---

### Remote

<p class="tdb-f">A remote repository.</p>

**Class:** `Remote`

**Super class:** `Repository`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `remote_url` | `xsd:string` | The url of the remote repository. |

---

### Repository

<p class="tdb-f">A repository.</p>

**Class:** `Repository`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `head` | `layer:Layer` | The current most recent layer of the repository. |
| `name` | `xsd:string` | The name of the repository. |

---

### layer:Layer

<p class="tdb-f">A layer object which has the identifier used in storage.</p>

**Class:** `layer:Layer`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `layer:identifier` | `xsd:string` | The layer identifier. |

---