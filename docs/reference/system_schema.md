# The System schema

This is the System schema in which resides all information regarding capabilities, users, organizations, databases and available actions.

**Authored by:** Gavin Mendel-Gleason, Matthijs van Otterdijk

---

### Action

<p class="tdb-f">The exhaustive list of actions which are available to roles.</p>

**Class:** `Action`

---

### Capability

<p class="tdb-f">A capability is a set of roles combined with a rescource over which those roles hold.</p>

**Class:** `Capability`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `scope` | `Resource` | The resource over which the role holds. |
| `role` | `Role` | The set of roles the capability has access to. |

---

### Database

<p class="tdb-f">A database.</p>

**Class:** `Database`

**Super class:** `Resource`

---

### DatabaseState

<p class="tdb-f">The current system transaction state of a database. Only the 'finalized' state is a consistent state, all others indicate that database construction failed.</p>

**Class:** `DatabaseState`

---

### Organization

<p class="tdb-f">An organisation.</p>

**Class:** `Organization`

**Super class:** `Resource`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `name` | `undefined` | The name of the organization. |
| `child` | `Organization` | The set of children of the organization. |
| `database` | `Database` | The set of databases controlled by the organization. |

---

### Resource

<p class="tdb-f">A named resource.</p>

**Class:** `Resource`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `name` | `xsd:string` | The name of the resource. |

---

### Role

<p class="tdb-f">Roles are named collections of actions which can be provided to a capability.</p>

**Class:** `Role`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `name` | `xsd:string` | The name of the role. |
| `action` | `Action` | The set of actions associated with the role. |

---

### SystemDatabase

<p class="tdb-f">The special system database.</p>

**Class:** `SystemDatabase`

**Super class:** `Database`

---

### User

<p class="tdb-f">A database user.</p>

**Class:** `User`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `name` | `xsd:string` | The users name. |
| `key_hash` | `xsd:string` | An optional key hash for authentication. |
| `capability` | `Capability` | A set of capabilities which the user has access to. |

---

### UserDatabase

<p class="tdb-f">A normal user database.</p>

**Class:** `UserDatabase`

**Super class:** `Database`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `label` | `xsd:string` | The label name of the database. |
| `comment` | `xsd:string` | A comment associated with the database. |
| `creation_date` | `xsd:dateTime` | The time of creation of the database. |
| `state` | `DatabaseState` | The system transaction state of the database. |

---