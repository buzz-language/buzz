import * as Constants from './constants';
import * as Utilities from './utilities';

interface GetDataParams {
  route: string;
  key: string | null;
  body: any;
}

interface UserData {
  id: string;
  key: string;
  type?: string;
  domain?: string;
  email?: string;
  password?: string;
  data?: any;
  fields?: any;
  src?: string;
  orderBy?: string;
  file?: File;
  user_id?: string;
}

interface UploadData {
  domain: string;
  file: File;
  key: string;
}

export async function getData({ route, key, body }: GetDataParams, qualifier = 'data') {
  let result;
  try {
    const response = await fetch(route, {
      method: 'POST',
      headers: {
        ...(key && { 'X-API-KEY': key }),
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(body),
    });
    result = await response.json();
  } catch (e) {
    return null;
  }

  if (!result) {
    return null;
  }

  if (result.error) {
    return null;
  }

  if (!result[qualifier]) {
    return null;
  }

  return result;
}

export async function onUserListData({ key }: Pick<UserData, 'key'>) {
  const route = `${Constants.API}/data`;
  const body = {};
  return await getData({ route, key, body });
}

export async function onUserDeleteData({ id, key }: Pick<UserData, 'id' | 'key'>) {
  const route = `${Constants.API}/data/delete`;
  const body = { id };
  return await getData({ route, key, body });
}

export async function onRefreshDocuments({ key, type, domain }: Pick<UserData, 'key' | 'type' | 'domain'>) {
  const route = `${Constants.API}/documents`;
  const body = { type, domain };
  return await getData({ route, key, body });
}

export async function onGetDocumentById({ id }: Pick<UserData, 'id'>) {
  const route = `${Constants.API}/documents/${id}`;
  const body = {};
  return await getData({ route, key: null, body });
}

export async function onUserCreateDocument({ key, type, domain }: Pick<UserData, 'key' | 'type' | 'domain'>) {
  const route = `${Constants.API}/documents/create`;
  const body = { type, domain };
  return await getData({ route, key, body });
}

export async function onDeleteDocumentById({ id, key }: Pick<UserData, 'id' | 'key'>) {
  const route = `${Constants.API}/documents/delete`;
  const body = { id };
  return await getData({ route, key, body });
}

export async function onUpdateDocumentById({ id, key, data }: Pick<UserData, 'id' | 'key' | 'data'>) {
  const route = `${Constants.API}/documents/update`;
  const body = { id, data };
  return await getData({ route, key, body });
}

export async function onPublicUserAuthenticate({ email, password }: Pick<UserData, 'email' | 'password'>) {
  const route = `${Constants.API}/users/authenticate`;
  const body = { email, password };
  return getData({ route, key: null, body }, 'user');
}

export async function onPublicUserForgotPassword({ email }: Pick<UserData, 'email'>) {
  const route = `${Constants.API}/users/reset-password`;
  const body = { email, source: 'wireframes.internet.dev' };
  return getData({ route, key: null, body }, 'success');
}

export async function onUserChangePassword({ key, password }: Pick<UserData, 'key' | 'password'>) {
  const route = `${Constants.API}/users/update-viewer-password`;
  const body = { password };
  return getData({ route, key, body });
}

export async function onUserRegenerateAPIKey({ email, password }: Pick<UserData, 'email' | 'password'>) {
  const route = `${Constants.API}/users/regenerate-key`;
  const body = { email, password };
  return getData({ route, key: null, body }, 'user');
}

export async function onUserUnsubscribeServices({ key }: Pick<UserData, 'key'>) {
  const route = `${Constants.API}/users/subscriptions/unsubscribe`;
  const body = null;
  return getData({ route, key, body }, 'user');
}

export async function onRefreshPosts({ key, type, user_id }: Pick<UserData, 'key' | 'type' | 'user_id'>) {
  const route = `${Constants.API}/posts`;
  const body = { type, user_id };
  return await getData({ route, key, body });
}

export async function onUserCreatePost({ id, key, src, type }: Pick<UserData, 'id' | 'key' | 'src' | 'type'>) {
  const route = `${Constants.API}/posts/create`;
  const body = { type, fields: { fileId: id, public: true }, src };
  return getData({ route, key, body });
}

export async function onUserCreateThread({ fields, key, src, type }: Pick<UserData, 'fields' | 'key' | 'src' | 'type'>) {
  const route = `${Constants.API}/posts/create`;
  const body = { fields, src, type };
  return getData({ route, key, body });
}

export async function onUserDeletePost({ id, key }: Pick<UserData, 'id' | 'key'>) {
  const route = `${Constants.API}/posts/delete`;
  const body = { id };
  return getData({ route, key, body });
}

export async function onUserListThreadReplies({ id, key, orderBy }: Pick<UserData, 'id' | 'key' | 'orderBy'>) {
  const route = `${Constants.API}/posts/all-thread-replies`;
  const body = { id, orderBy };
  return getData({ route, key, body });
}

export async function onUserListThreads({ key, orderBy }: Pick<UserData, 'key' | 'orderBy'>) {
  const route = `${Constants.API}/posts/all-threads`;
  const body = { orderBy };
  return getData({ route, key, body });
}

export async function onUserUploadDataGCS({ domain, file, key }: UploadData) {
  if (!file) {
    return { error: 'No file provided' };
  }

  let signedResult;
  const name = file.name;
  const type = file.type;
  const size = file.size;

  if (size > Constants.MAX_SIZE_BYTES) {
    return { error: 'File size exceeds 15mb limit' };
  }

  try {
    const route = `${Constants.API}/data/generate-presigned-url-gcs`;
    const body = { domain, type, file: name, size };
    signedResult = await getData({ route, key, body }, 'uploadURL');
  } catch (e) {
    return null;
  }

  try {
    await fetch(signedResult.uploadURL, {
      method: 'PUT',
      headers: {
        'Content-Type': 'application/octet-stream',
      },
      body: file,
    });
  } catch (e) {
    return null;
  }

  return signedResult;
}

export async function onUserUploadDataS3({ domain, file, key }: UploadData) {
  if (!file) {
    return { error: 'No file provided' };
  }

  let signedResult;
  const name = file.name;
  const type = file.type;
  const size = file.size;

  if (size > Constants.MAX_SIZE_BYTES) {
    return { error: 'File size exceeds 15mb limit' };
  }

  try {
    const route = `${Constants.API}/data/generate-presigned-url`;
    const body = { domain, type, file: name, size };
    signedResult = await getData({ route, key, body }, 'uploadURL');
  } catch (e) {
    return null;
  }

  try {
    await fetch(signedResult.uploadURL, {
      method: 'PUT',
      body: file,
    });
  } catch (e) {
    return null;
  }

  return signedResult;
}
